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
    ScopedSymbolTable named_values;

    llvm::Value* generate(Program&);

    llvm::Value* generate(Function&);
    llvm::Value* generateFunction(Function&, std::function<llvm::Value*()>);

    llvm::Value* generate(Parameter&);
    llvm::Value* generate(Block&);
    llvm::Value* generate(Definition&);
    llvm::Value* generate(BinaryOperation&);
    llvm::Value* generate(UnaryOperation&);
    llvm::Value* generate(IfElseExpr&);
    llvm::Value* generate(WhileExpr&);
    llvm::Value* generate(FunctionCall&);
    llvm::Value* generate(Variable&);
    llvm::Value* generate(BoolValue&);
    llvm::Value* generate(IntValue&);
    llvm::Value* generate(FloatValue&);
    llvm::Value* generate(NullValue&);
    llvm::Value* generate(BlankExpr&);

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

    llvm::AllocaInst* create_entry_block_alloca(llvm::Function*, llvm::Type*,
                                                const std::string&);

    llvm::Type* get_builtin_type(const std::string&);
    llvm::Type* type_check(const std::string&, llvm::Type*);

    llvm::Value* __llvm_value;

    void accept(Program&) override;
    void accept(Function&) override;
    void accept(Parameter&) override;
    void accept(Block&) override;
    void accept(Definition&) override;
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
};

}

}

}

#endif //FUNLANG_CODEGEN_H
