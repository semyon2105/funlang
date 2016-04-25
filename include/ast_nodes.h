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

struct Program;
struct Parameter;
struct Function;
struct Expression;
struct Block;
struct TypeId;
struct Definition;
struct BinaryOperation;
struct UnaryOperation;
struct IfElseExpr;
struct WhileExpr;
struct FunctionCall;
struct LValue;
struct Variable;
struct ArrayAccess;
struct BoolValue;
struct IntValue;
struct FloatValue;
struct ArrayLiteral;
struct NullValue;
struct BlankExpr;

struct Visitor
{
    virtual void accept(Program&) = 0;
    virtual void accept(Function&) = 0;
    virtual void accept(Parameter&) = 0;
    virtual void accept(Block&) = 0;
    virtual void accept(TypeId&) = 0;
    virtual void accept(Definition&) = 0;
    virtual void accept(BinaryOperation&) = 0;
    virtual void accept(UnaryOperation&) = 0;
    virtual void accept(IfElseExpr&) = 0;
    virtual void accept(WhileExpr&) = 0;
    virtual void accept(FunctionCall&) = 0;
    virtual void accept(Variable&) = 0;
    virtual void accept(ArrayAccess&) = 0;
    virtual void accept(BoolValue&) = 0;
    virtual void accept(IntValue&) = 0;
    virtual void accept(FloatValue&) = 0;
    virtual void accept(ArrayLiteral&) = 0;
    virtual void accept(NullValue&) = 0;
    virtual void accept(BlankExpr&) = 0;
};

struct Node
{
    virtual void accept(Visitor&) = 0;
};

struct Program : Node
{
    Program(std::vector<std::unique_ptr<Function>> functions);

    const std::vector<std::unique_ptr<Function>> functions;

    void accept(Visitor&) override;
};

struct Function : Node
{
    Function(std::string name,
             std::vector<std::unique_ptr<Parameter>> params,
             std::unique_ptr<TypeId> return_type,
             std::unique_ptr<Expression> body);

    const std::string name;
    const std::vector<std::unique_ptr<Parameter>> params;
    const std::unique_ptr<TypeId> return_type;
    const std::unique_ptr<Expression> body;

    void accept(Visitor&) override;
};

struct Parameter : Node
{
    Parameter(std::string name, std::unique_ptr<TypeId> type);

    const std::string name;
    const std::unique_ptr<TypeId> type;

    void accept(Visitor&) override;
};

struct Expression : Node {};

struct Block : Expression
{
    Block(std::vector<std::unique_ptr<Expression>> expressions);

    const std::vector<std::unique_ptr<Expression>> exprs;

    void accept(Visitor&) override;
};

struct TypeId : Node
{
    enum class Kind { Single, Array };

    TypeId(std::string name, Kind kind);

    const std::string name;
    const Kind kind;

    void accept(Visitor&) override;
};

struct Definition : Expression
{
    Definition(std::string name,
               std::unique_ptr<TypeId> type,
               std::unique_ptr<Expression> rhs);

    const std::string name;
    const std::unique_ptr<TypeId> type;
    const std::unique_ptr<Expression> rhs;

    void accept(Visitor&) override;
};

struct BinaryOperation : Expression
{
    enum class Kind
    {
        Assign,
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

    const std::unique_ptr<Expression> lhs;
    const Kind kind;
    const std::unique_ptr<Expression> rhs;

    void accept(Visitor&) override;
};

struct UnaryOperation : Expression
{
    enum class Kind { Not, Minus };

    static const std::map<Kind, const std::string> kind_strings;

    UnaryOperation(Kind kind, std::unique_ptr<Expression> expr);

    const Kind kind;
    const std::unique_ptr<Expression> expr;

    void accept(Visitor&) override;
};

struct IfElseExpr : Expression
{
public:
    IfElseExpr(
            std::unique_ptr<Expression> condition,
            std::unique_ptr<Expression> if_body,
            std::unique_ptr<Expression> else_body = nullptr);

    const std::unique_ptr<Expression> condition;
    const std::unique_ptr<Expression> if_body;
    const std::unique_ptr<Expression> else_body;

    void accept(Visitor&) override;
};

struct WhileExpr : Expression
{
    WhileExpr(std::unique_ptr<Expression> condition,
              std::unique_ptr<Expression> body);

    const std::unique_ptr<Expression> condition;
    const std::unique_ptr<Expression> body;

    void accept(Visitor&) override;
};

struct FunctionCall : Expression
{
    FunctionCall(std::string func_name,
                 std::vector<std::unique_ptr<Expression>> args);

    const std::string callee_name;
    const std::vector<std::unique_ptr<Expression>> args;

    void accept(Visitor&) override;
};

struct LValue : Expression {};

struct Variable : LValue
{
    Variable(std::string name);

    const std::string name;

    void accept(Visitor&) override;
};

struct ArrayAccess : LValue
{
    ArrayAccess(std::string name, std::unique_ptr<Expression> index_expr);

    const std::string name;
    const std::unique_ptr<Expression> index_expr;

    void accept(Visitor&) override;
};

struct Literal : Expression {};

struct BoolValue : Literal
{
    BoolValue(bool value);

    const bool value;

    void accept(Visitor&) override;
};

struct IntValue : Literal
{
    IntValue(int value);

    const int value;

    void accept(Visitor&) override;
};

struct FloatValue : Literal
{
    FloatValue(double value);

    const double value;

    void accept(Visitor&) override;
};

struct ArrayLiteral : Literal
{
    ArrayLiteral(std::vector<std::unique_ptr<Literal>> elements);

    const std::vector<std::unique_ptr<Literal>> elements;

    void accept(Visitor&) override;
};

struct NullValue : Literal
{
    void accept(Visitor&) override;
};

struct BlankExpr : Expression
{
    void accept(Visitor&) override;
};

}

}

#endif //FUNLANG_ASTNODES_H
