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

static const std::string PLACEHOLDER_TYPE = "DYNAMIC_TYPE";

class Program;
class Parameter;
class Function;
class Block;
class Expression;
class Definition;
class Assignment;
class BinaryOperation;
class BinaryOperationRest;
class UnaryOperation;
class IfExpr;
class WhileExpr;
class FunctionCall;
class Variable;
class BoolValue;
class IntValue;
class FloatValue;
class NullValue;

struct Visitor
{
    virtual void accept(Program&) = 0;
    virtual void accept(Function&) = 0;
    virtual void accept(Parameter&) = 0;
    virtual void accept(Block&) = 0;
    virtual void accept(Definition&) = 0;
    virtual void accept(Assignment&) = 0;
    virtual void accept(BinaryOperation&) = 0;
    virtual void accept(BinaryOperationRest&) = 0;
    virtual void accept(UnaryOperation&) = 0;
    virtual void accept(IfExpr&) = 0;
    virtual void accept(WhileExpr&) = 0;
    virtual void accept(FunctionCall&) = 0;
    virtual void accept(Variable&) = 0;
    virtual void accept(BoolValue&) = 0;
    virtual void accept(IntValue&) = 0;
    virtual void accept(FloatValue&) = 0;
    virtual void accept(NullValue&) = 0;
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
             std::unique_ptr<Block> body);

    const std::string& name() const;
    const std::vector<std::unique_ptr<Parameter>>& parameters() const;
    const std::string& return_type() const;
    Block* body() const;

    void accept(Visitor&) override;

private:
    const std::string name_;
    const std::vector<std::unique_ptr<Parameter>> params_;
    const std::string return_type_;
    const std::unique_ptr<Block> body_;
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

class Block : public Node
{
public:
    Block(std::vector<std::unique_ptr<Expression>> exprs);

    const std::vector<std::unique_ptr<Expression>>& expressions() const;

    void accept(Visitor&) override;

private:
    const std::vector<std::unique_ptr<Expression>> exprs;
};

struct Expression : public Node
{
    virtual const std::string& type() const = 0;
};

class Definition : public Expression
{
public:
    Definition(std::string var_name, std::string var_type, std::unique_ptr<Expression> expr);

    const std::string& type() const override { return PLACEHOLDER_TYPE; };

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

    const std::string& type() const override { return PLACEHOLDER_TYPE; };

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
                    std::unique_ptr<BinaryOperationRest> rest);

    const std::string& type() const override { return PLACEHOLDER_TYPE; };

    Expression* lhs() const;
    BinaryOperationRest* rest() const;

    void accept(Visitor&) override;

private:
    const std::unique_ptr<Expression> lhs_;
    const std::unique_ptr<BinaryOperationRest> rest_;
};

class BinaryOperationRest : public Node
{
public:
    BinaryOperationRest(BinaryOperation::Kind kind,
                        std::unique_ptr<Expression> rhs,
                        std::unique_ptr<BinaryOperationRest> rest);

    BinaryOperation::Kind kind() const;
    Expression* rhs() const;
    BinaryOperationRest* rest() const;

    void accept(Visitor&) override;

private:
    const BinaryOperation::Kind kind_;
    const std::unique_ptr<Expression> rhs_;
    const std::unique_ptr<BinaryOperationRest> rest_;
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

    const std::string& type() const override { return PLACEHOLDER_TYPE; };

    Kind kind() const;
    Expression* expression() const;

    void accept(Visitor&) override;

private:
    const Kind kind_;
    const std::unique_ptr<Expression> expr;
};

class IfExpr : public Expression
{
public:
    IfExpr(std::unique_ptr<Expression> condition, std::unique_ptr<Block> body);

    const std::string& type() const override { return PLACEHOLDER_TYPE; };

    Expression* condition() const;
    Block* body() const;

    void accept(Visitor&) override;

private:
    const std::unique_ptr<Expression> cond;
    const std::unique_ptr<Block> body_;
};

class WhileExpr : public Expression
{
public:
    WhileExpr(std::unique_ptr<Expression> condition, std::unique_ptr<Block> body);

    const std::string& type() const override { return PLACEHOLDER_TYPE; };

    Expression* condition() const;
    Block* body() const;

    void accept(Visitor&) override;

private:
    const std::unique_ptr<Expression> cond;
    const std::unique_ptr<Block> body_;
};

class FunctionCall : public Expression
{
public:
    FunctionCall(std::string func_name,
                 std::vector<std::unique_ptr<Expression>> args);

    const std::string& type() const override { return PLACEHOLDER_TYPE; };

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

    const std::string& type() const override { return PLACEHOLDER_TYPE; };

    const std::string& name() const;

    void accept(Visitor&) override;

private:
    const std::string name_;
};

struct BoolValue : public Expression
{
    BoolValue(bool value);

    const std::string& type() const override;

    const bool value;

    void accept(Visitor&) override;
};

struct IntValue : public Expression
{
    IntValue(int value);

    const std::string& type() const override;

    const int value;

    void accept(Visitor&) override;
};

struct FloatValue : public Expression
{
    FloatValue(double value);

    const std::string& type() const override;

    const double value;

    void accept(Visitor&) override;
};

struct NullValue : public Expression
{
    NullValue();

    const std::string& type() const override;

    void accept(Visitor&) override;
};

}

}

#endif //FUNLANG_ASTNODES_H
