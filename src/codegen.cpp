#include <memory>

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
struct EmptyBlockError                  : CodegenError {};
struct TypeMismatchError                : CodegenError {};
struct NoSuchTypeError                  : CodegenError {};
struct InvalidBinaryOperationError      : CodegenError {};

llvm::Value* codegen(ProgramAST& program_ast, LLVMContext& context)
{
    Module module{"module", context};

    /*
  TheModule = llvm::make_unique<Module>("my cool jit", getGlobalContext());
  TheModule->setDataLayout(TheJIT->getTargetMachine().createDataLayout());

  // Create a new pass manager attached to it.
  TheFPM = llvm::make_unique<FunctionPassManager>(TheModule.get());

  // Provide basic AliasAnalysis support for GVN.
  TheFPM.add(createBasicAliasAnalysisPass());
  // Do simple "peephole" optimizations and bit-twiddling optzns.
  TheFPM.add(createInstructionCombiningPass());
  // Reassociate expressions.
  TheFPM.add(createReassociatePass());
  // Eliminate Common SubExpressions.
  TheFPM.add(createGVNPass());
  // Simplify the control flow graph (deleting unreachable blocks, etc).
  TheFPM.add(createCFGSimplificationPass());

  TheFPM.doInitialization();
    */

    CodegenVisitor visitor{*program_ast.root(), context, module};
    return visitor.result();
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

// Поиск alloca переменной в таблицах символов, начиная с самой вложенной
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

CodegenVisitor::CodegenVisitor(Node& node, LLVMContext& context, Module& module)
    : root{node},
      context{context},
      module{module},
      builder{context},
      named_values{}
{
}

llvm::Value* CodegenVisitor::result()
{
    static bool cached = false;
    if (!cached) {
        root.accept(*this);
        cached = true;
    }
    return latest_result;
}

struct CodegenVisitor::ScopeGuard
{
    std::unique_ptr<ScopedSymbolTable>& this_scope_table;
    std::unique_ptr<ScopedSymbolTable> other_scope_table;

    ScopeGuard(std::unique_ptr<ScopedSymbolTable>& this_scope_table)
        : this_scope_table{this_scope_table},
          other_scope_table{std::make_unique<ScopedSymbolTable>(this_scope_table.get())}
    {
        this_scope_table.swap(other_scope_table);
    }

    ~ScopeGuard()
    {
        this_scope_table.swap(other_scope_table);
    }
};

llvm::Value* CodegenVisitor::generate(Node& node)
{
    node.accept(*this);
    return latest_result;
}

void CodegenVisitor::accept(Program& p)
{
}

void CodegenVisitor::accept(AST::Function& f)
{
    if (module.getFunction(f.name())) {
        throw FunctionAlreadyDefinedError{};
    }

    std::vector<Type*> param_types(f.parameters().size());
    for (const auto& param : f.parameters()) {
        Type* static_type = builtin_type_from_string(param->type_name());
        param_types.push_back(static_type);
    }
    Type* static_return_type = builtin_type_from_string(f.return_type());
    FunctionType* func_type =
            FunctionType::get(
                static_return_type,
                param_types, false);
    llvm::Function* func =
            llvm::Function::Create(
                func_type,
                llvm::Function::InternalLinkage,
                f.name(), &module);
    size_t pos = 0;
    for (auto& arg : func->args()) {
        arg.setName(f.parameters()[pos++]->name());
    }

    BasicBlock* bb = BasicBlock::Create(context, "entry", func);
    builder.SetInsertPoint(bb);
    {
        // make new scope for function arguments variables
        auto scope_guard = named_values.make_inner_scope();

        for (auto& arg : func->args()) {
            named_values[arg.getName()] = &arg;
        }

        Value* return_value = generate(*f.body());
        Type* static_type = builtin_type_from_string(f.return_type());
        if (return_value) {
            if (static_type != return_value->getType()) {
                throw TypeMismatchError{};
            }
            builder.CreateRet(return_value);
        }
        else {
            if (static_type != Type::getVoidTy(context)) {
                throw TypeMismatchError{};
            }
            builder.CreateRetVoid();
        }
        llvm::verifyFunction(*func);
        latest_result = func;
    }
}

void CodegenVisitor::accept(Parameter&)
{
}

void CodegenVisitor::accept(Block& b)
{
    auto scope_guard = named_values.make_inner_scope();

    if (b.expressions().empty()) {
        throw EmptyBlockError{};
    }      

    for (const auto& expr : b.expressions()) {
        if (expr) {
            latest_result = generate(*expr);
        }
    }
}

void CodegenVisitor::accept(Definition& d)
{
    llvm::Function* function = builder.GetInsertBlock()->getParent();

    Value* var_value = generate(*d.expression());
    if (!var_value) {
        return;
    }

    Type* type = builtin_type_from_string(d.var_type());
    AllocaInst* alloca = create_entry_block_alloca(function, type, d.var_name());
    builder.CreateStore(var_value, alloca);
    named_values.get_from_current_scope(d.var_name()) = alloca;
}

void CodegenVisitor::accept(Assignment& a)
{
    CodegenVisitor rhs_expr_codegen{context, module, builder, &named_values};
    a.expression()->accept(rhs_expr_codegen);
    Value* rhs_expr = rhs_expr_codegen.result_;
    if (!rhs_expr) {
        result = nullptr;
        return;
    }
    Value* variable = named_values[a.variable_name()];
    if (!variable) {
        throw UndefinedVariableError{};
    }
    builder->CreateStore(rhs_expr, variable);
    result = variable;
}

Value* CodegenVisitor::match_bool_binop(BinaryOperation::Kind kind,
                                        Value* lhs, Value* rhs)
{
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

Value* CodegenVisitor::match_int_binop(BinaryOperation::Kind kind,
                                       Value* lhs, Value* rhs)
{
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

/*
Value* CodegenVisitor::match_float_binop(BinaryOperation::Kind kind,
                                        Value* lhs, Value* rhs,
                                        Type* lhs_type, Type* rhs_type)
{
    if (lhs_type->isDoubleTy() && rhs_type->isDoubleTy()) {
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
    }
    return nullptr;
}
*/

Value* CodegenVisitor::match_numeric_binop(BinaryOperation::Kind kind,
                                           Value* lhs, Value* rhs)
{
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

void CodegenVisitor::accept(BinaryOperation& b)
{
    Value* lhs = generate(*b.lhs());
    Value* rhs = generate(*b.rhs());
    if (!lhs || !rhs) {
        throw CodegenError{};
    }

    Value* binop;
    if ((binop = match_bool_binop(b.kind(), lhs, rhs)) ||
        (binop = match_int_binop(b.kind(), lhs, rhs)) ||
        (binop = match_numeric_binop(b.kind(), lhs, rhs)))
    {
        latest_result = binop;
    }
    else {
        throw InvalidExpressionError{};
    }
    throw UnreachableCodeError{};
}

void CodegenVisitor::accept(UnaryOperation& u)
{
}

void CodegenVisitor::accept(IfElseExpr& i)
{
    Value* cond = generate(*i.condition());

    if (cond->getType()->isIntegerTy(1)) {
        throw TypeError{};
    }

    llvm::Function* func = builder.GetInsertBlock()->getParent();

    BasicBlock* if_body_bb = BasicBlock::Create(context, "if_body", func);
    BasicBlock* else_body_bb = BasicBlock::Create(context, "else_body");
    BasicBlock* after_ifelse_bb = BasicBlock::Create(context, "after_ifelse");

    builder.CreateCondBr(cond, if_body_bb, else_body_bb);

    builder.SetInsertPoint(if_body_bb);
    Value* if_br = generate(*i.if_body());
    if (!if_br) {
        throw InvalidExpressionError{};
    }
    builder.CreateBr(after_ifelse_bb);
    if_body_bb = builder.GetInsertBlock();

    func->getBasicBlockList().push_back(else_body_bb);

    builder.SetInsertPoint(else_body_bb);
    Value* else_br = generate(*i.else_body());
    if (!else_br) {
        throw InvalidExpressionError{};
    }
    builder.CreateBr(after_ifelse_bb);
    else_body_bb = builder.GetInsertBlock();

    func->getBasicBlockList().push_back(after_ifelse_bb);

    builder.SetInsertPoint(after_ifelse_bb);

    std::vector<Value*&> branches = {if_br, else_br};

    Type* phi_type = common_type(branches);
    if (!phi_type) {
        branches = convert_numeric_to_float(std::move(branches));
        phi_type = Type::getDoubleTy(context);
    }

    PHINode* phi_node =
            builder.CreatePHI(phi_type, 2, "ifelsetmp");
    phi_node->addIncoming(branches[0], if_body_bb);
    phi_node->addIncoming(branches[1], else_body_bb);
    latest_result = phi_node;
}

void CodegenVisitor::accept(WhileExpr& w)
{
}

void CodegenVisitor::accept(FunctionCall& f)
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
            throw CodegenError{};
        }
    }

    latest_result = builder.CreateCall(callee, actual_args, "fcalltmp");
}

void CodegenVisitor::accept(Variable& v)
{
    Value* value = named_values[v.name()];
    if (!value) {
        throw UndefinedVariableError{};
    }
    latest_result = builder.CreateLoad(value, v.name().c_str());
}

void CodegenVisitor::accept(BoolValue& v)
{
    latest_result = ConstantInt::get(
                Type::getInt1Ty(context), v.value, false);
}

void CodegenVisitor::accept(IntValue& v)
{
    latest_result = ConstantInt::get(Type::getInt32Ty(context),
                                     v.value, true);
}

void CodegenVisitor::accept(FloatValue& v)
{
    latest_result = ConstantFP::get(Type::getDoubleTy(context), v.value);
}

void CodegenVisitor::accept(NullValue& v)
{
    latest_result = Constant::getNullValue(Type::getVoidTy(context));
}

void CodegenVisitor::accept(BlankExpr& b)
{
    latest_result = Constant::getNullValue(Type::getVoidTy(context));
}

AllocaInst* CodegenVisitor::create_entry_block_alloca(
        llvm::Function* function,
        llvm::Type* var_type,
        const std::string& var_name)
{
    IRBuilder<> tmp_builder{
                &function->getEntryBlock(),
                function->getEntryBlock().begin()};

    return tmp_builder.CreateAlloca(var_type, 0, var_name.c_str());
}

llvm::Type* CodegenVisitor::builtin_type_from_string(const std::string& name)
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

Type* CodegenVisitor::common_type(const std::vector<Value*>& values)
{
    if (values.empty()) {
        return nullptr;
    }
    Type* common = (*std::cbegin(values))->getType();
    for (const auto& value : values) {
        if (value->getType() != common) {
            return nullptr;
        }
    }
    return common;
}

std::vector<Value*> CodegenVisitor::convert_numeric_to_float(std::vector<Value*>&& values)
{
    auto is_number = [](Value* v) {
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
