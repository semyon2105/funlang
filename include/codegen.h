#ifndef FUNLANG_CODEGEN_H
#define FUNLANG_CODEGEN_H

#include <unordered_map>
#include <vector>

#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Module.h"

#include "ast_nodes.h"
#include "program_ast.h"

namespace Funlang
{

namespace AST
{

llvm::Value* codegen(ProgramAST&, llvm::LLVMContext& = llvm::getGlobalContext());

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

class CodegenVisitor : private Visitor
{
public:
    CodegenVisitor(Node& node, llvm::LLVMContext&, llvm::Module&);

    llvm::Value* result();

private:
    struct ScopeGuard;

    Node& root;
    llvm::LLVMContext& context;
    llvm::Module& module;

    llvm::IRBuilder<> builder;
    ScopedSymbolTable named_values;

    llvm::Value* latest_result;

    llvm::Value* generate(Node&);

    void accept(Program&) override;
    void accept(Function&) override;
    void accept(Parameter&) override;
    void accept(Block&) override;
    void accept(Definition&) override;
    void accept(Assignment&) override;
    void accept(BinaryOperation&) override;
    void accept(UnaryOperation&) override;
    void accept(IfElseExpr&) override;
    void accept(WhileExpr&) override;
    void accept(FunctionCall&) override;
    void accept(Variable&) override;
    void accept(BoolValue&) override;
    void accept(IntValue&) override;
    void accept(FloatValue&) override;
    void accept(NullValue&) override;
    void accept(BlankExpr&) override;

    llvm::Value* match_bool_binop(BinaryOperation::Kind,
                                  llvm::Value*, llvm::Value*);
    llvm::Value* match_int_binop(BinaryOperation::Kind,
                                  llvm::Value*, llvm::Value*);
    llvm::Value* match_numeric_binop(BinaryOperation::Kind,
                                  llvm::Value*, llvm::Value*);

    llvm::Type* common_type(const std::vector<llvm::Value*>&);

    std::vector<llvm::Value*> convert_numeric_to_float(std::vector<llvm::Value*>&&);

    llvm::AllocaInst* create_entry_block_alloca(llvm::Function*, llvm::Type*,
                                                const std::string&);
    llvm::Type* builtin_type_from_string(const std::string&);
};

}

}

}

#endif //FUNLANG_CODEGEN_H
