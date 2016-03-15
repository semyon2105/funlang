#ifndef FUNLANG_ASTNODES_H
#define FUNLANG_ASTNODES_H

#include <memory>
#include <string>
#include <unordered_map>
#include <vector>

namespace Funlang
{

namespace AST
{

struct Visitor
{

};

struct Node
{
    // virtual void accept(Visitor& v) const = 0;

    virtual ~Node();

protected:
    Node();
};

class Program : public Node
{
public:
    Program();
    Program(std::vector<std::unique_ptr<Function>> functions,
            std::unique_ptr<Function> entrypoint);

    const std::vector<std::unique_ptr<Function>>& functions() const;
    std::unique_ptr<Function> entrypoint() const;

private:
    std::vector<std::unique_ptr<Function>> functions_;
    std::unique_ptr<Function> entrypoint_;
};

struct Parameter
{
    Parameter(const std::string& name, Expression::Type type);

    const std::string name;
    const Expression::Type type;
};

class Function : public Node
{
public:
    Function(const std::string& name,
             std::vector<Parameter> params,
             Expression::Type return_type,
             std::unique_ptr<Block> body);

    std::string name() const;
    const std::vector<Parameter>& parameters() const;
    const Expression::Type type() const;
    Block* body() const;

private:
    const std::string name_;
    const std::vector<Parameter> params;
    const Expression::Type return_type;
    std::unique_ptr<Block> body_;
};

class Block : public Node
{
public:
    Block();
    Block(std::vector<std::unique_ptr<Expression>> exprs);

    const std::vector<std::unique_ptr<Expression>>& expressions() const;
    auto& symbol_table() -> std::unordered_map<std::string, Variable*>;

private:
    std::unordered_map<std::string, Variable*> sym_table;
    std::vector<std::unique_ptr<Expression>> exprs;
};

class Expression : public Node
{
public:
    Expression();

    enum class Type
    {
        Bool,
        Error,
        Int,
        Float,
        Null
    };

    virtual auto& symbol_table() -> std::unordered_map<std::string, Variable*>;
    virtual const Type& type() const;

    virtual ~Expression();

private:
    std::unordered_map<std::string, Variable*> sym_table;
};

class Definition : public Expression
{
public:
    Definition(std::unique_ptr<Variable> var, std::unique_ptr<Expression> expr);

    const Type& type() const override;

    Variable* variable() const;
    Expression* expression() const;

private:
    std::unique_ptr<Variable> var;
    std::unique_ptr<Expression> expr;
};

class Assignment : public Expression
{
public:
    Assignment(std::unique_ptr<Variable> var, std::unique_ptr<Expression> expr);

    const Type& type() const override;

    Variable* variable() const;
    Expression* expression() const;

private:
    std::unique_ptr<Variable> var;
    std::unique_ptr<Expression> expr;
};

class BinaryOperation : public Expression
{

public:
    enum class Kind
    {
        Add,
        Sub,
        Mul,
        Div,
        Less,
        Greater,
        Equal,
        NotEq,
        LeEq,
        GrEq
    };

    BinaryOperation(Kind kind, std::unique_ptr<Expression> lhs_expr, std::unique_ptr<Expression> rhs_expr);

    const Type& type() const override;

    const Kind& kind() const;
    Expression* lhs() const;
    Expression* rhs() const;

private:
    Kind kind_;
    std::unique_ptr<Expression> lhs_expr;
    std::unique_ptr<Expression> rhs_expr;
};

class UnaryOperation : public Expression
{
public:
    enum class Kind
    {
        Not,
        Minus
    };

    UnaryOperation(Kind kind, std::unique_ptr<Expression> expr)
    {

    }

    const Type& type() const override;

    const Kind& kind() const;
    Expression* expression() const;

private:
    Kind kind_;
    std::unique_ptr<Expression> expr_;
};

class IfExpr : public Expression
{
public:
    IfExpr(std::unique_ptr<Expression> condition, std::unique_ptr<Block> body);

    const Type& type() const override;

    Expression* condition() const;
    Block* body() const;

private:
    std::unique_ptr<Expression> cond;
    std::unique_ptr<Block> body_;
};

class WhileExpr : public Expression
{
public:
    WhileExpr(std::unique_ptr<Expression> condition, std::unique_ptr<Block> body);

    const Type& type() const override;

    Expression* condition() const;
    Block* body() const;

private:
    std::unique_ptr<Expression> cond;
    std::unique_ptr<Block> body_;
};

class FunctionCall : public Expression
{
public:
    FunctionCall(const std::string& func_name, std::vector<std::unique_ptr<Expression>>&& args);

    const Type& type() const override;

    std::string function_name() const;
    const std::vector<std::unique_ptr<Expression>>& arguments();

private:
    const std::string& func_name;
    const std::vector<std::unique_ptr<Expression>> args;

};

class Variable : public Expression
{
public:
    Variable(const std::string& name);

    const Type& type() const override;

    std::string name() const;

private:
    const std::string name_;
};

struct BoolValue : public Expression
{
public:
    BoolValue(bool value);

    const Type& type() const override;

    bool value() const;

private:
    const bool value_;
};

struct IntValue : public Expression
{
public:
    IntValue(int value);

    const Type& type() const override;

    int value() const;

private:
    const int value_;
};

struct FloatValue : public Expression
{
public:
    FloatValue(double value);

    const Type& type() const override;

    double value() const;

private:
    const double value_;
};

}

}

#endif //FUNLANG_ASTNODES_H
