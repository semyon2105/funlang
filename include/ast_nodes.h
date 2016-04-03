#ifndef FUNLANG_ASTNODES_H
#define FUNLANG_ASTNODES_H

#include <memory>
#include <string>
#include <map>
#include <vector>

#include "token.h"

namespace Funlang
{

namespace AST
{

class Program;
class Parameter;
class Function;
class Expression;
class Block;
class Definition;
class Assignment;
class BinaryOperation;
class UnaryOperation;
class IfElseExpr;
class WhileExpr;
class FunctionCall;
class Variable;
class BoolValue;
class IntValue;
class FloatValue;
class NullValue;
class BlankExpr;

struct Visitor
{
    virtual void accept(Program&) = 0;
    virtual void accept(Function&) = 0;
    virtual void accept(Parameter&) = 0;
    virtual void accept(Block&) = 0;
    virtual void accept(Definition&) = 0;
    virtual void accept(Assignment&) = 0;
    virtual void accept(BinaryOperation&) = 0;
    virtual void accept(UnaryOperation&) = 0;
    virtual void accept(IfElseExpr&) = 0;
    virtual void accept(WhileExpr&) = 0;
    virtual void accept(FunctionCall&) = 0;
    virtual void accept(Variable&) = 0;
    virtual void accept(BoolValue&) = 0;
    virtual void accept(IntValue&) = 0;
    virtual void accept(FloatValue&) = 0;
    virtual void accept(NullValue&) = 0;
    virtual void accept(BlankExpr&) = 0;
};

struct Node
{
    virtual void accept(Visitor&) = 0;
};

class Program : public Node
{
public:
    Program(std::vector<std::unique_ptr<Function>> functions);

    const std::vector<std::unique_ptr<Function>>& functions() const;

    void accept(Visitor&) override;

private:
    const std::vector<std::unique_ptr<Function>> functions_;
};

class Function : public Node
{
public:
    Function(std::string name,
             std::vector<std::unique_ptr<Parameter>> params,
             std::string return_type,
             std::unique_ptr<Expression> body);

    const std::string& name() const;
    const std::vector<std::unique_ptr<Parameter>>& parameters() const;
    const std::string& return_type() const;
    Expression* body() const;

    void accept(Visitor&) override;

private:
    const std::string name_;
    const std::vector<std::unique_ptr<Parameter>> params_;
    const std::string return_type_;
    const std::unique_ptr<Expression> body_;
};

class Parameter : public Node
{
public:
    Parameter(std::string name, std::string type_name);

    const std::string& name() const;
    const std::string& type_name() const;

    void accept(Visitor&) override;

private:
    const std::string name_;
    const std::string type_name_;
};

struct Expression : public Node
{
};

class Block : public Expression
{
public:
    Block(std::vector<std::unique_ptr<Expression>> exprs);

    const std::vector<std::unique_ptr<Expression>>& expressions() const;

    void accept(Visitor&) override;

private:
    const std::vector<std::unique_ptr<Expression>> exprs;
};

class Definition : public Expression
{
public:
    Definition(std::string var_name, std::string var_type, std::unique_ptr<Expression> expr);

    const std::string& var_name() const;
    const std::string& var_type() const;

    Expression* expression() const;

    void accept(Visitor&) override;

private:
    const std::string var_name_;
    const std::string var_type_;
    const std::unique_ptr<Expression> expr;
};

class Assignment : public Expression
{
public:
    Assignment(std::string variable_name, std::unique_ptr<Expression> expression);

    const std::string& variable_name() const;
    Expression* expression() const;

    void accept(Visitor&) override;

private:
    const std::string var_name;
    const std::unique_ptr<Expression> expr;
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

    static const std::map<Kind, const std::string> kind_strings;
    static Kind from_token_kind(const Token::Kind&);

    BinaryOperation(std::unique_ptr<Expression> lhs,
                    Kind kind,
                    std::unique_ptr<Expression> rhs);

    Expression* lhs() const;
    Kind kind() const;
    Expression* rhs() const;

    void accept(Visitor&) override;

private:
    const std::unique_ptr<Expression> lhs_;
    const Kind kind_;
    const std::unique_ptr<Expression> rhs_;
};

class UnaryOperation : public Expression
{
public:
    enum class Kind
    {
        Not,
        Minus
    };

    static const std::map<Kind, const std::string> kind_strings;

    UnaryOperation(Kind kind, std::unique_ptr<Expression> expr);

    Kind kind() const;
    Expression* expression() const;

    void accept(Visitor&) override;

private:
    const Kind kind_;
    const std::unique_ptr<Expression> expr;
};

class IfElseExpr : public Expression
{
public:
    IfElseExpr(
            std::unique_ptr<Expression> condition,
            std::unique_ptr<Expression> if_body,
            std::unique_ptr<Expression> else_body = nullptr);

    Expression* condition() const;
    Expression* if_body() const;
    Expression* else_body() const;

    void accept(Visitor&) override;

private:
    const std::unique_ptr<Expression> cond;
    const std::unique_ptr<Expression> if_body_;
    const std::unique_ptr<Expression> else_body_;
};

class WhileExpr : public Expression
{
public:
    WhileExpr(std::unique_ptr<Expression> condition,
              std::unique_ptr<Expression> body);

    Expression* condition() const;
    Expression* body() const;

    void accept(Visitor&) override;

private:
    const std::unique_ptr<Expression> cond;
    const std::unique_ptr<Expression> body_;
};

class FunctionCall : public Expression
{
public:
    FunctionCall(std::string func_name,
                 std::vector<std::unique_ptr<Expression>> args);

    const std::string& function_name() const;
    const std::vector<std::unique_ptr<Expression>>& arguments() const;

    void accept(Visitor&) override;

private:
    const std::string func_name;
    const std::vector<std::unique_ptr<Expression>> args;

};

class Variable : public Expression
{
public:
    Variable(std::string name);

    const std::string& name() const;

    void accept(Visitor&) override;

private:
    const std::string name_;
};

struct BoolValue : Expression
{
    BoolValue(bool value);

    const bool value;

    void accept(Visitor&) override;
};

struct IntValue : Expression
{
    IntValue(int value);

    const int value;

    void accept(Visitor&) override;
};

struct FloatValue : Expression
{
    FloatValue(double value);

    const double value;

    void accept(Visitor&) override;
};

struct NullValue : Expression
{
    NullValue();

    void accept(Visitor&) override;
};

struct BlankExpr : Expression
{
    void accept(Visitor&) override;
};

}

}

#endif //FUNLANG_ASTNODES_H
