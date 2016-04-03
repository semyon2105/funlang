#include <memory>

#include <llvm/IR/TypeBuilder.h>
#include <llvm/IR/Verifier.h>

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

void AST::codegen(Program& program, LLVMContext& context)
{
    Codegen visitor{context};
    visitor.generate(static_cast<Node&>(program));
    visitor.get_module().dump();
}

ScopedSymbolTable::ScopedSymbolTable()
{
    tables.push_back(std::unordered_map<std::string, Value*>{});
}

struct ScopedSymbolTable::ScopeGuard
{
    std::vector<std::unordered_map<std::string, Value*>>& tables;

    ScopeGuard(std::vector<std::unordered_map<std::string, Value*>>& tables)
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

// Поиск llvm::Value* переменной в таблицах символов, начиная с самой вложенной
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

    // всегда nullptr
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
    params.emplace_back(std::make_unique<Parameter>("value", "float"));
    AST::Function print_float_prototype {
        "print",
        std::move(params),
        "void",
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
                    true, GlobalValue::PrivateLinkage, format_cstr, "format");
        Constant* zero =
                Constant::getNullValue(IntegerType::getInt32Ty(context));
        Constant* format_ref = ConstantExpr::getGetElementPtr(
                    gv->getType(), gv, std::vector<Constant*>{zero, zero});


        CallInst* call = builder.CreateCall(
                    printf_func, {format_ref, named_values["value"]});
        call->setTailCall(false);
        return nullptr;
    });

    AST::Function input_float_prototype {
        "input",
        {},
        "float",
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
                    true, GlobalValue::PrivateLinkage, format_cstr, "str");
        Constant* zero =
                Constant::getNullValue(IntegerType::getInt32Ty(context));
        Constant* format_ref = ConstantExpr::getGetElementPtr(
                    format_gv->getType(), format_gv, std::vector<Constant*>{zero, zero});

        llvm::Function* function = builder.GetInsertBlock()->getParent();

        GlobalVariable* float_gv = new GlobalVariable(
                    module, Type::getDoubleTy(context),
                    false, GlobalValue::PrivateLinkage,
                    ConstantFP::getNullValue(Type::getDoubleTy(context)),
                    "input_float");
        Value* float_ref = builder.CreateGEP(float_gv, {zero, zero});

        CallInst* call = builder.CreateCall(
                    scanf_func, {format_ref, float_ref});
        call->setTailCall(false);

        return float_gv;
    });
}

Value* Codegen::generate(Program& p)
{
    Value* entrypoint = nullptr;

    // add print, input
    add_utility_functions();

    for (const auto& func : p.functions()) {
        if (func->name() == "main") {
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
        return generate(*f.body());
    });
}

Value* Codegen::generateFunction(AST::Function& f, std::function<Value*()> body_gen)
{
    if (module.getFunction(f.name())) {
        throw FunctionAlreadyDefinedError{};
    }

    std::vector<Type*> param_types;
    std::transform(std::cbegin(f.parameters()), std::cend(f.parameters()),
                   std::back_inserter(param_types),
                   [this](const auto& param) {
        return builtin_type_from_string(param->type_name());
    });

    Type* static_return_type = builtin_type_from_string(f.return_type());
    FunctionType* func_type =
            FunctionType::get(
                static_return_type,
                param_types, false);
    llvm::Function* func =
            llvm::Function::Create(
                func_type,
                llvm::Function::ExternalLinkage,
                f.name(), &module);

    size_t pos = 0;
    for (auto& arg : func->args()) {
        arg.setName(f.parameters()[pos++]->name());
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

        Value* return_value = body_gen();
        Type* static_type = builtin_type_from_string(f.return_type());
        if (return_value) {
            if (static_type != return_value->getType()) {
                throw TypeMismatchError{};
            }
            if (return_value->getType()->isVoidTy()) {
                builder.CreateRetVoid();
            }
            else {
                builder.CreateRet(return_value);
            }
        }
        else {
            if (static_type != Type::getVoidTy(context)) {
                throw TypeMismatchError{};
            }
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
    for (const auto& expr : b.expressions()) {
        expr_value = generate(*expr);
    }
    return expr_value;
}

Value* Codegen::generate(Definition& d)
{
    llvm::Function* function = builder.GetInsertBlock()->getParent();

    Value* rvalue = generate(*d.expression());
    if (!rvalue) {
        throw UnexpectedBlankExpressionError{};
    }
    Type* type = builtin_type_from_string(d.var_type());
    if (type != rvalue->getType()) {
        throw TypeMismatchError{};
    }
    AllocaInst* alloca = create_entry_block_alloca(function, type, d.var_name());
    builder.CreateStore(rvalue, alloca);
    named_values.value_from_current_scope(d.var_name()) = alloca;
    return rvalue;
}

Value* Codegen::generate(Assignment& a)
{
    Value* rvalue = generate(*a.expression());
    if (!rvalue) {
        throw UnexpectedBlankExpressionError{};
    }
    Value* variable = named_values[a.variable_name()];
    if (!variable) {
        throw UndefinedVariableError{};
    }
    builder.CreateStore(rvalue, variable);
    return rvalue;
}

Value* Codegen::generate(BinaryOperation& b)
{
    Value* lhs = generate(*b.lhs());
    Value* rhs = generate(*b.rhs());
    if (!lhs || !rhs) {
        throw UnexpectedBlankExpressionError{};
    }

    Value* binop;
    if ((binop = match_bool_binop(b.kind(), lhs, rhs)) ||
        (binop = match_int_binop(b.kind(), lhs, rhs)) ||
        (binop = match_numeric_binop(b.kind(), lhs, rhs)))
    {
        return binop;
    }
    else {
        throw InvalidExpressionError{};
    }
}

Value* Codegen::generate(UnaryOperation& u)
{
    Value* expr = generate(*u.expression());
    if (!expr) {
        throw UnexpectedBlankExpressionError{};
    }
    if (u.kind() == UnaryOperation::Kind::Minus) {
        expr = builder.CreateNeg(expr, "negtmp", false, false);
    }
    else if (u.kind() == UnaryOperation::Kind::Not) {
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

    Value* cond = generate(*i.condition());

    if (!cond->getType()->isIntegerTy(1)) {
        throw TypeError{};
    }

    BasicBlock* if_body_bb = BasicBlock::Create(context, "if_body", func);
    BasicBlock* else_body_bb = BasicBlock::Create(context, "else_body");
    BasicBlock* after_ifelse_bb = BasicBlock::Create(context, "after_ifelse");

    builder.CreateCondBr(cond, if_body_bb, else_body_bb);

    builder.SetInsertPoint(if_body_bb);
    Value* if_br = generate(*i.if_body());
    builder.CreateBr(after_ifelse_bb);
    if_body_bb = builder.GetInsertBlock();

    builder.SetInsertPoint(else_body_bb);

    Value* else_br = nullptr;
    if (i.else_body()) {
        func->getBasicBlockList().push_back(else_body_bb);
        else_br = generate(*i.else_body());
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
    Value* condition = generate(*w.condition());
    if (!condition) {
        throw CodegenError{};
    }
    if (!condition->getType()->isIntegerTy(1)) {
        throw TypeError{};
    }
    header_bb = builder.GetInsertBlock();

    builder.CreateCondBr(condition, loop_bb, loop_exit_bb);

    func->getBasicBlockList().push_back(loop_bb);
    builder.SetInsertPoint(loop_bb);
    Value* body = generate(*w.body());
    if (!body) {
        throw CodegenError{};
    }

    AllocaInst* loop_result_alloca =
            create_entry_block_alloca(func, body->getType(), "loopresult");

    builder.CreateStore(body, loop_result_alloca);
    builder.CreateBr(header_bb);
    loop_bb = builder.GetInsertBlock();

    func->getBasicBlockList().push_back(loop_exit_bb);
    builder.SetInsertPoint(loop_exit_bb);

    return builder.CreateLoad(loop_result_alloca, "loopresulttmp");
}

Value* Codegen::generate(FunctionCall& f)
{
    llvm::Function* callee = module.getFunction(f.function_name());
    if (!callee) {
        throw UndefinedReferenceError{};
    }
    if (callee->arg_size() != f.arguments().size()) {
        throw IncorrectNumberOfArgumentsError{};
    }

    std::vector<Value*> actual_args;
    for (const auto& arg : f.arguments()) {
        actual_args.push_back(generate(*arg));
        if (!actual_args.back()) {
            throw UnexpectedBlankExpressionError{};
        }
    }

    return builder.CreateCall(callee, actual_args);
}

Value* Codegen::generate(Variable& v)
{
    Value* value = named_values[v.name()];
    if (!value) {
        throw UndefinedVariableError{};
    }
    return builder.CreateLoad(value, v.name().c_str());
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

Value* Codegen::generate(NullValue&)
{
    return Constant::getNullValue(Type::getVoidTy(context));
}

Value* Codegen::generate(BlankExpr&)
{
    return nullptr;
}

Value* Codegen::match_bool_binop(BinaryOperation::Kind kind,
                                        Value* lhs, Value* rhs)
{
    if (!lhs || !rhs) {
        return nullptr;
    }
    auto types = {lhs->getType(), rhs->getType()};
    auto is_bool = [](Type* t) { return t->isIntegerTy(1); };
    // build binop if lhs and rhs are bools
    if (std::all_of(std::cbegin(types), std::cend(types), is_bool)) {
        switch (kind) {
        case BinaryOperation::Kind::Equal:
            return builder.CreateICmpEQ(lhs, rhs, "cmpeqtmp");
        case BinaryOperation::Kind::NotEq:
            return builder.CreateICmpNE(lhs, rhs, "cmpnetmp");
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
                                       Value* lhs, Value* rhs)
{
    if (!lhs || !rhs) {
        return nullptr;
    }
    auto types = {lhs->getType(), rhs->getType()};
    auto is_int = [](Type* t) { return t->isIntegerTy(32); };
    // build if both operands are ints
    if (std::all_of(std::cbegin(types), std::cend(types), is_int)) {
        switch (kind) {
        case BinaryOperation::Kind::Add:
            return builder.CreateAdd(lhs, rhs, "addtmp");
        case BinaryOperation::Kind::Sub:
            return builder.CreateSub(lhs, rhs, "subtmp");
        case BinaryOperation::Kind::Mul:
            return builder.CreateMul(lhs, rhs, "multmp");
        case BinaryOperation::Kind::Div:
            return builder.CreateSDiv(lhs, rhs, "sdivtmp");
        case BinaryOperation::Kind::Less:
            return builder.CreateICmpSLT(lhs, rhs, "cmpslttmp");
        case BinaryOperation::Kind::Greater:
            return builder.CreateICmpSGT(lhs, rhs, "cmpsgttmp");
        case BinaryOperation::Kind::Equal:
            return builder.CreateICmpEQ(lhs, rhs, "cmpeqtmp");
        case BinaryOperation::Kind::NotEq:
            return builder.CreateICmpNE(lhs, rhs, "cmpnetmp");
        case BinaryOperation::Kind::LeEq:
            return builder.CreateICmpSLE(lhs, rhs, "cmpsletmp");
        case BinaryOperation::Kind::GrEq:
            return builder.CreateICmpSGE(lhs, rhs, "cmpsgetmp");
        }
    }

    return nullptr;
}

Value* Codegen::match_numeric_binop(BinaryOperation::Kind kind,
                                           Value* lhs, Value* rhs)
{
    if (!lhs || !rhs) {
        return nullptr;
    }
    auto numbers = convert_numeric_to_float(std::vector<Value*>{lhs, rhs});
    lhs = numbers[0];
    rhs = numbers[1];
    switch (kind) {
    case BinaryOperation::Kind::Add:
        return builder.CreateFAdd(lhs, rhs, "addtmp");
    case BinaryOperation::Kind::Sub:
        return builder.CreateFSub(lhs, rhs, "subtmp");
    case BinaryOperation::Kind::Mul:
        return builder.CreateFMul(lhs, rhs, "multmp");
    case BinaryOperation::Kind::Div:
        return builder.CreateFDiv(lhs, rhs, "sdivtmp");
    case BinaryOperation::Kind::Less:
        return builder.CreateFCmpULT(lhs, rhs, "cmpulttmp");
    case BinaryOperation::Kind::Greater:
        return builder.CreateFCmpUGT(lhs, rhs, "cmpugttmp");
    case BinaryOperation::Kind::Equal:
        return builder.CreateFCmpUEQ(lhs, rhs, "cmpueqtmp");
    case BinaryOperation::Kind::NotEq:
        return builder.CreateFCmpUNE(lhs, rhs, "cmpunetmp");
    case BinaryOperation::Kind::LeEq:
        return builder.CreateFCmpULE(lhs, rhs, "cmpuletmp");
    case BinaryOperation::Kind::GrEq:
        return builder.CreateFCmpUGE(lhs, rhs, "cmpugetmp");
    }
    return nullptr;
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

llvm::Type* Codegen::builtin_type_from_string(const std::string& name)
{
    if (name == "bool") {
        return Type::getInt1Ty(context);
    }
    if (name == "int") {
        return Type::getInt32Ty(context);
    }
    if (name == "float") {
        return Type::getDoubleTy(context);
    }
    if (name == "void") {
        return Type::getVoidTy(context);
    }
    throw NoSuchTypeError{};
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

std::vector<Value*> Codegen::convert_numeric_to_float(std::vector<Value*>&& values)
{
    auto is_number = [](Value* v) {
        if (!v) return false;
        Type* t = v->getType();
        return t->isIntegerTy(32) || t->isDoubleTy();
    };

    if (!std::all_of(std::cbegin(values), std::cend(values), is_number)) {
        throw TypeError{};
    }

    for (Value*& value : values) {
        value = builder.CreateSIToFP(
                    value, Type::getDoubleTy(context), "sitofptmp");
    }
    return values;
}

llvm::Value* Codegen::generate(Node& node)
{
    node.accept(*this);
    return __llvm_value;
}
void Codegen::accept(Program& n)         { __llvm_value = generate(n); };
void Codegen::accept(AST::Function& n)   { __llvm_value = generate(n); };
void Codegen::accept(Parameter& n)       { __llvm_value = generate(n); };
void Codegen::accept(Block& n)           { __llvm_value = generate(n); };
void Codegen::accept(Definition& n)      { __llvm_value = generate(n); };
void Codegen::accept(Assignment& n)      { __llvm_value = generate(n); };
void Codegen::accept(BinaryOperation& n) { __llvm_value = generate(n); };
void Codegen::accept(UnaryOperation& n)  { __llvm_value = generate(n); };
void Codegen::accept(IfElseExpr& n)      { __llvm_value = generate(n); };
void Codegen::accept(WhileExpr& n)       { __llvm_value = generate(n); };
void Codegen::accept(FunctionCall& n)    { __llvm_value = generate(n); };
void Codegen::accept(Variable& n)        { __llvm_value = generate(n); };
void Codegen::accept(BoolValue& n)       { __llvm_value = generate(n); };
void Codegen::accept(IntValue& n)        { __llvm_value = generate(n); };
void Codegen::accept(FloatValue& n)      { __llvm_value = generate(n); };
void Codegen::accept(NullValue& n)       { __llvm_value = generate(n); };
void Codegen::accept(BlankExpr& n)       { __llvm_value = generate(n); };
