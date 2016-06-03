#ifndef FUNLANG_CODEGEN_H
#define FUNLANG_CODEGEN_H

#include <unordered_map>

#include "llvm/CodeGen/ValueTypes.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Module.h"

#include "ast_nodes.h"

namespace Funlang
{

namespace AST
{

std::string codegen(Program&, llvm::LLVMContext& = llvm::getGlobalContext());

namespace impl
{

class ScopedSymbolTable
{
    struct ScopeGuard;

public:
    ScopedSymbolTable();

    llvm::Value*& value(const std::string& key);
    llvm::Value*& value_from_current_scope(const std::string& key);

    llvm::Value*& operator[](const std::string& key);

    ScopeGuard make_inner_scope();

private:
    std::vector<std::unordered_map<std::string, llvm::Value*>> tables;
};

struct CodegenError;

//struct UndefinedVariableError;
struct TypeError;
struct UnreachableCodeError;
struct InvalidExpressionError;
struct UndefinedReferenceError;
struct IncorrectNumberOfArgumentsError;
struct FunctionAlreadyDefinedError;
struct TypeMismatchError;
struct NoSuchTypeError;
struct InvalidBinaryOperationError;
struct InvalidUnaryOperationError;
struct MainFunctionNotDefinedError;
struct NonPositiveDimSizeError;
struct ZeroLengthArrayError;
struct UnspecifiedTypeError;

class Codegen : private Visitor
{
public:
    Codegen(llvm::LLVMContext&);

    llvm::Value* generate(Node&);

    llvm::Module& get_module();

private:
    llvm::LLVMContext& context;

    llvm::Module module;
    llvm::IRBuilder<> builder;
    ScopedSymbolTable variables;

    Node* current_node = nullptr;

    llvm::Value* generate(const Program&);

    llvm::Value* generate(const Function&);
    llvm::Value* generateFunction(const Function&, std::function<llvm::Value*()>);

    llvm::Value* generate(const Block&);
    llvm::Value* generate(const Definition&);
    llvm::Value* generate(const BinaryOperation&);
    llvm::Value* generate(const UnaryOperation&);
    llvm::Value* generate(const IfElseExpr&);
    llvm::Value* generate(const WhileExpr&);
    llvm::Value* generate(const FunctionCall&);
    llvm::Value* generate(const Variable&);
    llvm::Value* generate(const ArrayAccess&);
    llvm::Value* generate(const BoolValue&);
    llvm::Value* generate(const IntValue&);
    llvm::Value* generate(const FloatValue&);
    llvm::Value* generate(const ArrayExpr&);
    llvm::Value* generate(const NullValue&);
    llvm::Value* generate(const BlankExpr&);

    void add_utility_functions();
    llvm::Function* add_main_function();

    llvm::Value* match_binop(BinaryOperation::Kind,
                             llvm::Value*, llvm::Value*);
    llvm::Value* match_bool_binop(BinaryOperation::Kind,
                                  llvm::Value*, llvm::Value*);
    llvm::Value* match_int_binop(BinaryOperation::Kind,
                                 llvm::Value*, llvm::Value*);
    llvm::Value* match_numeric_binop(BinaryOperation::Kind,
                                     llvm::Value*, llvm::Value*);
    llvm::Value* match_assignment_binop(BinaryOperation::Kind,
                                        llvm::Value*, llvm::Value*);

    llvm::Value* rvalue(llvm::Value*);
    std::vector<llvm::Value*> rvalue(std::vector<llvm::Value*>);

    llvm::Type* common_type(const std::vector<llvm::Value*>&);

    std::vector<llvm::Value*> convert_numeric_to_float(std::vector<llvm::Value*>);

    llvm::AllocaInst* create_entry_block_alloca(
            llvm::Function*, llvm::Type*, const std::string&);

    llvm::Value* default_initialize(const StaticTypeId*);

    llvm::Type* get_primitive_type(const PrimitiveTypeId&);
    llvm::Type* get_array_type(const ArrayTypeId&);
    llvm::Type* get_type(const StaticTypeId*);

    llvm::Type* type_check(llvm::Type*, llvm::Type*);
    llvm::Type* type_check(const StaticTypeId*, llvm::Type*);

    template<typename ErrorT, typename... Args>
    ErrorT error(Args&&... args)
    {
        static_assert(std::is_base_of<CodegenError, ErrorT>::value,
                      "ErrorT must be derived from AST::impl::CodegenError");
        auto error = ErrorT{std::forward<Args>(args)...};
        if (current_node) {
            error.node = current_node;
        }
        return error;
    }

    void accept(Program&) override;
    void accept(Function&) override;
    void accept(Parameter&) override;
    void accept(Block&) override;
    void accept(StaticTypeId&) override;
    void accept(PrimitiveTypeId&) override;
    void accept(ArrayTypeId&) override;
    void accept(Definition&) override;
    void accept(BinaryOperation&) override;
    void accept(UnaryOperation&) override;
    void accept(IfElseExpr&) override;
    void accept(WhileExpr&) override;
    void accept(FunctionCall&) override;
    void accept(Variable&) override;
    void accept(ArrayAccess&) override;
    void accept(BoolValue&) override;
    void accept(IntValue&) override;
    void accept(FloatValue&) override;
    void accept(ArrayExpr&) override;
    void accept(NullValue&) override;
    void accept(BlankExpr&) override;

    llvm::Value* latest_llvm_value = nullptr;
};

}

}

}

#endif //FUNLANG_CODEGEN_H
