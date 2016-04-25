#include <functional>
#include <memory>

#include <boost/core/demangle.hpp>

#include <llvm/IR/TypeBuilder.h>
#include <llvm/IR/Verifier.h>
#include <llvm/Support/raw_ostream.h>

#include "codegen.h"

using namespace llvm;

using namespace Funlang;
using namespace Funlang::AST;
using namespace Funlang::AST::impl;

struct CodegenError {};

struct UndefinedVariableError           : CodegenError {};
struct TypeError                        : CodegenError {};
struct UnreachableCodeError             : CodegenError {};
struct InvalidExpressionError           : CodegenError {};
struct UndefinedReferenceError          : CodegenError {};
struct IncorrectNumberOfArgumentsError  : CodegenError {};
struct FunctionAlreadyDefinedError      : CodegenError {};
struct TypeMismatchError                : CodegenError {};
struct NoSuchTypeError                  : CodegenError {};
struct InvalidBinaryOperationError      : CodegenError {};
struct UnexpectedBlankExpressionError   : CodegenError {};
struct MainFunctionNotDefined           : CodegenError {};

std::string AST::codegen(Program& program, LLVMContext& context)
{
    Codegen visitor{context};
    visitor.generate(static_cast<Node&>(program));
    std::string output;
    raw_string_ostream os {output};
    visitor.get_module().print(os, nullptr);
    return output;
}

ScopedSymbolTable::ScopedSymbolTable()
{
    tables.push_back(std::unordered_map<std::string, Value*>{});
}

struct ScopedSymbolTable::ScopeGuard
{
    std::vector<std::unordered_map<std::string, Value*>>& tables;

    explicit ScopeGuard(std::vector<std::unordered_map<std::string, Value*>>& tables)
        : tables{tables}
    {
        tables.push_back(std::unordered_map<std::string, Value*>{});
    }

    ~ScopeGuard()
    {
        tables.pop_back();
    }
};

ScopedSymbolTable::ScopeGuard ScopedSymbolTable::make_inner_scope()
{
    return ScopeGuard{tables};
}

Value*& ScopedSymbolTable::value(const std::string& key)
{
    for (auto table_iter = std::rbegin(tables);
         table_iter != std::rend(tables);
         ++table_iter)
    {
        Value*& alloca = (*table_iter)[key];
        if (alloca) {
            return (*table_iter)[key];
        }
    }

    return (*std::begin(tables))[key];
}

Value*& ScopedSymbolTable::value_from_current_scope(const std::string& key)
{
    return (*std::rbegin(tables))[key];
}

Value*& ScopedSymbolTable::operator[](const std::string& key)
{
    return value(key);
}

Codegen::Codegen(LLVMContext& context)
    : context{context},
      module{"module", context},
      builder{context},
      named_values{}
{

}

llvm::Module& Codegen::get_module()
{
    return module;
}

void Codegen::add_utility_functions()
{
    FunctionType* printf_scanf_type =
            FunctionType::get(
                Type::getInt32Ty(context), {Type::getInt8PtrTy(context)}, true);

    llvm::Function* printf_func =
            llvm::Function::Create(
                printf_scanf_type, llvm::Function::ExternalLinkage,
                "printf", &module);
    printf_func->setCallingConv(CallingConv::C);

    llvm::Function* scanf_func =
            llvm::Function::Create(
                printf_scanf_type, llvm::Function::ExternalLinkage,
                "scanf", &module);
    scanf_func->setCallingConv(CallingConv::C);

    std::vector<std::unique_ptr<Parameter>> params;
    params.emplace_back(std::make_unique<Parameter>(
                            "value",
                            std::make_unique<TypeId>(
                                "float", TypeId::Kind::Single)));
    AST::Function print_float_prototype {
        "print",
        std::move(params),
        std::make_unique<TypeId>("void", TypeId::Kind::Single),
        nullptr
    };
    generateFunction(print_float_prototype, [&]() {
        std::string format = "%lf\n";
        Constant* format_cstr =
                ConstantDataArray::getString(context, format);
        GlobalVariable* gv = new GlobalVariable(
                    module, ArrayType::get(
                        IntegerType::get(context, 8), format.length() + 1
                    ),
                    true, GlobalValue::PrivateLinkage,
                    format_cstr, "printf_format");
        Constant* zero =
                Constant::getNullValue(IntegerType::getInt32Ty(context));
        Value* format_ref = builder.CreateInBoundsGEP(gv, {zero, zero});

        Variable float_value_arg {"value"};
        CallInst* call = builder.CreateCall(
                    printf_func,
                    {format_ref, rvalue(generate(float_value_arg))}
        );
        call->setTailCall(false);
        return nullptr;
    });

    AST::Function input_float_prototype {
        "input",
        {},
        std::make_unique<TypeId>("float", TypeId::Kind::Single),
        nullptr
    };
    generateFunction(input_float_prototype, [&]() {
        std::string format = "%lf";
        Constant* format_cstr =
                ConstantDataArray::getString(context, format);
        GlobalVariable* format_gv = new GlobalVariable(
                    module, ArrayType::get(
                        IntegerType::get(context, 8), format.length() + 1
                    ),
                    true, GlobalValue::PrivateLinkage, format_cstr, "scanf_format");
        Constant* zero =
                Constant::getNullValue(IntegerType::getInt32Ty(context));
        Value* format_ref = builder.CreateInBoundsGEP(format_gv, {zero, zero});

        llvm::Function* function = builder.GetInsertBlock()->getParent();

        auto func = builder.GetInsertBlock()->getParent();
        AllocaInst* float_alloca = create_entry_block_alloca(
                    func, Type::getDoubleTy(context), "input_float");

        Value* float_ref = builder.CreateInBoundsGEP(float_alloca, {zero});

        CallInst* call = builder.CreateCall(
                    scanf_func, {format_ref, float_ref});
        call->setTailCall(false);

        return builder.CreateLoad(float_alloca, "floattmp");
    });
}

Value* Codegen::generate(Program& p)
{
    Value* entrypoint = nullptr;

    // add print, input
    add_utility_functions();

    for (const auto& func : p.functions) {
        if (func->name == "main") {
            entrypoint = generate(*func);
        }
        else {
            generate(*func);
        }
    }
    if (!entrypoint) {
        throw MainFunctionNotDefined{};
    }
    return entrypoint;
}

Value* Codegen::generate(AST::Function& f)
{
    return generateFunction(f, [this, &f]() {
        return generate(*f.body);
    });
}

Value* Codegen::generateFunction(AST::Function& f, std::function<Value*()> body_gen)
{
    if (module.getFunction(f.name)) {
        throw FunctionAlreadyDefinedError{};
    }

    std::vector<Type*> param_types;
    std::transform(std::cbegin(f.params), std::cend(f.params),
                   std::back_inserter(param_types),
                   [this](const auto& param) {
        return get_builtin_type(*param->type);
    });

    Type* static_return_type = get_builtin_type(*f.return_type);
    FunctionType* func_type =
            FunctionType::get(
                static_return_type,
                param_types, false);
    llvm::Function* func =
            llvm::Function::Create(
                func_type,
                llvm::Function::ExternalLinkage,
                f.name, &module);

    size_t pos = 0;
    for (auto& arg : func->args()) {
        arg.setName(f.params[pos++]->name);
    }

    BasicBlock* bb = BasicBlock::Create(context, "entry", func);
    builder.SetInsertPoint(bb);
    {
        auto scope_guard = named_values.make_inner_scope();

        for (auto& arg : func->args()) {
            AllocaInst* alloca =
                    create_entry_block_alloca(func, arg.getType(), arg.getName());

            builder.CreateStore(&arg, alloca);

            named_values.value_from_current_scope(arg.getName()) = alloca;
        }

        Value* return_value = rvalue(body_gen());
        if (return_value) {
            Type* type = type_check(*f.return_type, return_value->getType());
            if (type->isVoidTy()) {
                builder.CreateRetVoid();
            }
            else {
                builder.CreateRet(return_value);
            }
        }
        else {
            type_check(*f.return_type, Type::getVoidTy(context));
            builder.CreateRetVoid();
        }
    }
    llvm::verifyFunction(*func);
    return func;
}

Value* Codegen::generate(Parameter&)
{
    throw UnreachableCodeError{};
}

Value* Codegen::generate(Block& b)
{
    auto scope_guard = named_values.make_inner_scope();

    Value* expr_value = nullptr;
    for (const auto& expr : b.exprs) {
        expr_value = generate(*expr);
    }
    return expr_value;
}

Value* Codegen::generate(TypeId& t)
{
    throw UnreachableCodeError{};
}

Value* Codegen::generate(Definition& d)
{
    llvm::Function* function = builder.GetInsertBlock()->getParent();

    Value* rhs = rvalue(generate(*d.rhs));
    if (!rhs) {
        throw UnexpectedBlankExpressionError{};
    }
    Type* type = type_check(*d.type, rhs->getType());

    AllocaInst* alloca = create_entry_block_alloca(function, type, d.name);
    builder.CreateStore(rhs, alloca);
    named_values.value_from_current_scope(d.name) = alloca;

    return rhs;
}

Value* Codegen::generate(BinaryOperation& b)
{
    Value* lhs = generate(*b.lhs);
    Value* rhs = generate(*b.rhs);
    if (!lhs || !rhs) {
        throw UnexpectedBlankExpressionError{};
    }

    Value* binop = match_binop(b.kind, lhs, rhs);
    if (!binop) {
        throw InvalidExpressionError{};
    }
    return binop;
}

Value* Codegen::generate(UnaryOperation& u)
{
    Value* expr = rvalue(generate(*u.expr));
    if (!expr) {
        throw UnexpectedBlankExpressionError{};
    }
    if (u.kind == UnaryOperation::Kind::Minus) {
        expr = builder.CreateNeg(expr, "negtmp", false, false);
    }
    else if (u.kind == UnaryOperation::Kind::Not) {
        expr = builder.CreateNot(expr, "nottmp");
    }
    else {
        throw CodegenError{};
    }
    return expr;
}

Value* Codegen::generate(IfElseExpr& i)
{
    llvm::Function* func = builder.GetInsertBlock()->getParent();

    Value* cond = rvalue(generate(*i.condition));

    if (!cond->getType()->isIntegerTy(1)) {
        throw TypeError{};
    }

    BasicBlock* if_body_bb = BasicBlock::Create(context, "if_body", func);
    BasicBlock* else_body_bb = BasicBlock::Create(context, "else_body");
    BasicBlock* after_ifelse_bb = BasicBlock::Create(context, "after_ifelse");

    builder.CreateCondBr(cond, if_body_bb, else_body_bb);

    builder.SetInsertPoint(if_body_bb);
    Value* if_br = generate(*i.if_body);
    builder.CreateBr(after_ifelse_bb);
    if_body_bb = builder.GetInsertBlock();

    builder.SetInsertPoint(else_body_bb);

    Value* else_br = nullptr;
    if (i.else_body) {
        func->getBasicBlockList().push_back(else_body_bb);
        else_br = generate(*i.else_body);
        builder.CreateBr(after_ifelse_bb);
        else_body_bb = builder.GetInsertBlock();
    }
    else {
        func->getBasicBlockList().push_back(after_ifelse_bb);
        builder.SetInsertPoint(after_ifelse_bb);
        return if_br;
    }

    func->getBasicBlockList().push_back(after_ifelse_bb);
    builder.SetInsertPoint(after_ifelse_bb);

    std::vector<Value*> branches = {if_br, else_br};

    Type* phi_type = common_type(branches);
    if (!phi_type) {
        branches = convert_numeric_to_float(std::move(branches));
        phi_type = Type::getDoubleTy(context);
    }

    PHINode* phi_node =
            builder.CreatePHI(phi_type, 2, "ifelse");
    phi_node->addIncoming(branches[0], if_body_bb);
    phi_node->addIncoming(branches[1], else_body_bb);
    return phi_node;
}

Value* Codegen::generate(WhileExpr& w)
{
    llvm::Function* func = builder.GetInsertBlock()->getParent();

    BasicBlock* header_bb = BasicBlock::Create(context, "loopheader", func);
    BasicBlock* loop_bb = BasicBlock::Create(context, "loop");
    BasicBlock* loop_exit_bb = BasicBlock::Create(context, "loopexit");

    builder.CreateBr(header_bb);

    builder.SetInsertPoint(header_bb);
    Value* condition = rvalue(generate(*w.condition));
    if (!condition) {
        throw CodegenError{};
    }
    if (!condition->getType()->isIntegerTy(1)) {
        condition->getType()->dump();
        throw TypeError{};
    }
    header_bb = builder.GetInsertBlock();

    builder.CreateCondBr(condition, loop_bb, loop_exit_bb);

    func->getBasicBlockList().push_back(loop_bb);
    builder.SetInsertPoint(loop_bb);
    generate(*w.body);
    builder.CreateBr(header_bb);
    loop_bb = builder.GetInsertBlock();

    func->getBasicBlockList().push_back(loop_exit_bb);
    builder.SetInsertPoint(loop_exit_bb);

    return nullptr;
}

Value* Codegen::generate(FunctionCall& f)
{
    llvm::Function* callee = module.getFunction(f.callee_name);
    if (!callee) {
        throw UndefinedReferenceError{};
    }
    if (callee->arg_size() != f.args.size()) {
        throw IncorrectNumberOfArgumentsError{};
    }

    std::vector<Value*> actual_args;
    for (const auto& arg : f.args) {
        actual_args.push_back(rvalue(generate(*arg)));
        if (!actual_args.back()) {
            throw UnexpectedBlankExpressionError{};
        }
    }

    return builder.CreateCall(callee, actual_args);
}

Value* Codegen::generate(Variable& v)
{
    return named_values[v.name];
}

Value* Codegen::generate(ArrayAccess& a)
{

}

Value* Codegen::generate(BoolValue& v)
{
    return ConstantInt::get(Type::getInt1Ty(context), v.value, false);
}

Value* Codegen::generate(IntValue& v)
{
    return ConstantInt::get(Type::getInt32Ty(context), v.value, true);
}

Value* Codegen::generate(FloatValue& v)
{
    return ConstantFP::get(Type::getDoubleTy(context), v.value);
}

Value* Codegen::generate(ArrayLiteral& arr)
{
    // TODO
    return nullptr;
}

Value* Codegen::generate(NullValue&)
{
    return Constant::getNullValue(Type::getVoidTy(context));
}

Value* Codegen::generate(BlankExpr&)
{
    return nullptr;
}

Value* Codegen::match_binop(BinaryOperation::Kind kind,
                            Value* lhs, Value* rhs)
{
    Value* rval_rhs = rvalue(rhs);
    Value* assignment = match_assignment_binop(kind, lhs, rval_rhs);
    if (assignment) {
        return assignment;
    }

    Value* rval_lhs = rvalue(lhs);
    Value* binop = nullptr;
    if ((binop = match_bool_binop(kind, rval_lhs, rval_rhs)) ||
        (binop = match_int_binop(kind, rval_lhs, rval_rhs)) ||
        (binop = match_numeric_binop(kind, rval_lhs, rval_rhs)))
    {
        return binop;
    }
    return nullptr;
}

Value* Codegen::match_bool_binop(BinaryOperation::Kind kind,
                                 Value* rval_lhs, Value* rval_rhs)
{
    auto types = {rval_lhs->getType(), rval_rhs->getType()};
    auto is_bool = [](Type* t) { return t->isIntegerTy(1); };
    // build a binop if both lhs and rhs are bools
    if (std::all_of(std::cbegin(types), std::cend(types), is_bool)) {
        switch (kind) {
        case BinaryOperation::Kind::Equal:
            return builder.CreateICmpEQ(rval_lhs, rval_rhs, "cmpeqtmp");
        case BinaryOperation::Kind::NotEq:
            return builder.CreateICmpNE(rval_lhs, rval_rhs, "cmpnetmp");
        default:
            throw InvalidBinaryOperationError{};
        }
    }
    // don't build if none are bools
    else if (std::none_of(std::cbegin(types), std::cend(types), is_bool)) {
        return nullptr;
    }
    // type error if only one is a bool
    throw TypeError{};
}

Value* Codegen::match_int_binop(BinaryOperation::Kind kind,
                                Value* rval_lhs, Value* rval_rhs)
{
    auto types = {rval_lhs->getType(), rval_rhs->getType()};
    auto is_int = [](Type* t) { return t->isIntegerTy(32); };
    // build if both operands are ints
    if (std::all_of(std::cbegin(types), std::cend(types), is_int)) {
        switch (kind) {
        case BinaryOperation::Kind::Add:
            return builder.CreateAdd(rval_lhs, rval_rhs, "addtmp");
        case BinaryOperation::Kind::Sub:
            return builder.CreateSub(rval_lhs, rval_rhs, "subtmp");
        case BinaryOperation::Kind::Mul:
            return builder.CreateMul(rval_lhs, rval_rhs, "multmp");
        case BinaryOperation::Kind::Div:
            return builder.CreateSDiv(rval_lhs, rval_rhs, "sdivtmp");
        case BinaryOperation::Kind::Less:
            return builder.CreateICmpSLT(rval_lhs, rval_rhs, "cmpslttmp");
        case BinaryOperation::Kind::Greater:
            return builder.CreateICmpSGT(rval_lhs, rval_rhs, "cmpsgttmp");
        case BinaryOperation::Kind::Equal:
            return builder.CreateICmpEQ(rval_lhs, rval_rhs, "cmpeqtmp");
        case BinaryOperation::Kind::NotEq:
            return builder.CreateICmpNE(rval_lhs, rval_rhs, "cmpnetmp");
        case BinaryOperation::Kind::LeEq:
            return builder.CreateICmpSLE(rval_lhs, rval_rhs, "cmpsletmp");
        case BinaryOperation::Kind::GrEq:
            return builder.CreateICmpSGE(rval_lhs, rval_rhs, "cmpsgetmp");
        default:
            throw InvalidBinaryOperationError{};
        }
    }
    return nullptr;
}

Value* Codegen::match_numeric_binop(BinaryOperation::Kind kind,
                                    Value* rval_lhs, Value* rval_rhs)
{
    auto is_numeric = [](Type* t) {
        return t->isIntegerTy(32) || t->isDoubleTy();
    };

    if (!is_numeric(rval_lhs->getType())
        || !is_numeric(rval_rhs->getType())) {
        return nullptr;
    }
    auto numbers = convert_numeric_to_float(
                std::vector<Value*>{rval_lhs, rval_rhs});
    rval_lhs = numbers[0];
    rval_rhs = numbers[1];
    switch (kind) {
    case BinaryOperation::Kind::Add:
        return builder.CreateFAdd(rval_lhs, rval_rhs, "addtmp");
    case BinaryOperation::Kind::Sub:
        return builder.CreateFSub(rval_lhs, rval_rhs, "subtmp");
    case BinaryOperation::Kind::Mul:
        return builder.CreateFMul(rval_lhs, rval_rhs, "multmp");
    case BinaryOperation::Kind::Div:
        return builder.CreateFDiv(rval_lhs, rval_rhs, "sdivtmp");
    case BinaryOperation::Kind::Less:
        return builder.CreateFCmpULT(rval_lhs, rval_rhs, "cmpulttmp");
    case BinaryOperation::Kind::Greater:
        return builder.CreateFCmpUGT(rval_lhs, rval_rhs, "cmpugttmp");
    case BinaryOperation::Kind::Equal:
        return builder.CreateFCmpUEQ(rval_lhs, rval_rhs, "cmpueqtmp");
    case BinaryOperation::Kind::NotEq:
        return builder.CreateFCmpUNE(rval_lhs, rval_rhs, "cmpunetmp");
    case BinaryOperation::Kind::LeEq:
        return builder.CreateFCmpULE(rval_lhs, rval_rhs, "cmpuletmp");
    case BinaryOperation::Kind::GrEq:
        return builder.CreateFCmpUGE(rval_lhs, rval_rhs, "cmpugetmp");
    default:
        throw InvalidBinaryOperationError{};
    }
}

Value* Codegen::match_assignment_binop(BinaryOperation::Kind kind,
                                       Value* lhs, Value* rval_rhs)
{
    if (kind == BinaryOperation::Kind::Assign) {
        builder.CreateStore(rval_rhs, lhs);
        return rval_rhs;
    }
    return nullptr;
}

Value* Codegen::rvalue(Value* value)
{
    if (!value) {
        return nullptr;
    }
    Value* rval = nullptr;
    if (isa<AllocaInst>(value)) {
        rval = builder.CreateLoad(value);
    }
    return rval ? rval : value;
}

std::vector<Value*> Codegen::rvalue(std::vector<Value*> values)
{
    std::for_each(std::begin(values), std::end(values),
                  [this](Value* v) { return rvalue(v); });
    return values;
}


AllocaInst* Codegen::create_entry_block_alloca(
        llvm::Function* function,
        llvm::Type* var_type,
        const std::string& var_name)
{
    IRBuilder<> tmp_builder{
                &function->getEntryBlock(),
                function->getEntryBlock().begin()};

    return tmp_builder.CreateAlloca(var_type, nullptr, var_name.c_str());
}

llvm::Type* Codegen::get_builtin_type(const TypeId& type)
{
    // TODO: arrays
    if (type.name == "bool") {
        return Type::getInt1Ty(context);
    }
    if (type.name == "int") {
        return Type::getInt32Ty(context);
    }
    if (type.name == "float") {
        return Type::getDoubleTy(context);
    }
    if (type.name == "void") {
        return Type::getVoidTy(context);
    }
    throw NoSuchTypeError{};
}

llvm::Type* Codegen::type_check(const TypeId& type, Type* deducted)
{
    if (type.name == "") {
        return deducted;
    }
    Type* static_type = get_builtin_type(type);
    if (static_type != deducted) {
        throw TypeMismatchError{};
    }
    return static_type;
}

Type* Codegen::common_type(const std::vector<Value*>& values)
{
    if (!*std::cbegin(values)) {
        return nullptr;
    }
    Type* common = (*std::cbegin(values))->getType();
    for (const auto& value : values) {
        if (!value || value->getType() != common) {
            return nullptr;
        }
    }
    return common;
}

std::vector<Value*> Codegen::convert_numeric_to_float(std::vector<Value*> values)
{
    for (Value*& value : values) {
        value = builder.CreateSIToFP(
                    value, Type::getDoubleTy(context), "sitofptmp");
    }
    return values;
}

llvm::Value* Codegen::generate(Node& node)
{
    node.accept(*this);
    return latest_llvm_value;
}

void Codegen::accept(Program& n)         { latest_llvm_value = generate(n); }
void Codegen::accept(AST::Function& n)   { latest_llvm_value = generate(n); }
void Codegen::accept(Parameter& n)       { latest_llvm_value = generate(n); }
void Codegen::accept(Block& n)           { latest_llvm_value = generate(n); }
void Codegen::accept(TypeId& n)          { latest_llvm_value = generate(n); }
void Codegen::accept(Definition& n)      { latest_llvm_value = generate(n); }
void Codegen::accept(BinaryOperation& n) { latest_llvm_value = generate(n); }
void Codegen::accept(UnaryOperation& n)  { latest_llvm_value = generate(n); }
void Codegen::accept(IfElseExpr& n)      { latest_llvm_value = generate(n); }
void Codegen::accept(WhileExpr& n)       { latest_llvm_value = generate(n); }
void Codegen::accept(FunctionCall& n)    { latest_llvm_value = generate(n); }
void Codegen::accept(Variable& n)        { latest_llvm_value = generate(n); }
void Codegen::accept(ArrayAccess& n)     { latest_llvm_value = generate(n); }
void Codegen::accept(BoolValue& n)       { latest_llvm_value = generate(n); }
void Codegen::accept(IntValue& n)        { latest_llvm_value = generate(n); }
void Codegen::accept(FloatValue& n)      { latest_llvm_value = generate(n); }
void Codegen::accept(ArrayLiteral& n)    { latest_llvm_value = generate(n); }
void Codegen::accept(NullValue& n)       { latest_llvm_value = generate(n); }
void Codegen::accept(BlankExpr& n)       { latest_llvm_value = generate(n); }
