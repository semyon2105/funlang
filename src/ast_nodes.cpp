#include <utility>
#include <parse.h>

#include "ast_nodes.h"

using namespace Funlang::AST;

Program::Program(std::vector<std::unique_ptr<Function>> functions)
    : functions_{std::move(functions)}
{
}

const std::vector<std::unique_ptr<Function>>& Program::functions() const
{
    return functions_;
}

Function::Function(std::string name,
                   std::vector<std::unique_ptr<Parameter>> params,
                   std::string return_type,
                   std::unique_ptr<Expression> body)
    : name_{std::move(name)},
      params_{std::move(params)},
      return_type_{std::move(return_type)},
      body_{std::move(body)}
{
}

const std::string& Function::name() const
{
    return name_;
}

const std::vector<std::unique_ptr<Parameter>>& Function::parameters() const
{
    return params_;
}

const std::string& Function::return_type() const
{
    return return_type_;
}

Expression* Function::body() const
{
    return body_.get();
}

Parameter::Parameter(std::string name, std::string type_name)
        : name_{std::move(name)}, type_name_{std::move(type_name)}
{
}

const std::string& Parameter::name() const
{
    return name_;
}

const std::string& Parameter::type_name() const
{
    return type_name_;
}

Block::Block(std::vector<std::unique_ptr<Expression>> exprs)
    : exprs{std::move(exprs)}
{
}

const std::vector<std::unique_ptr<Expression>>& Block::expressions() const
{
    return exprs;
}

Definition::Definition(
        std::unique_ptr<LValue> lvalue,
        std::string type,
        std::unique_ptr<Expression> rhs)
    : lvalue_{std::move(lvalue)}, type_{std::move(type)}, rhs_{std::move(rhs)}
{
}

LValue* Definition::lvalue() const
{
    return lvalue_.get();
}

const std::string& Definition::type() const
{
    return type_;
}

Expression* Definition::rhs() const
{
    return rhs_.get();
}

BinaryOperation::BinaryOperation(
        std::unique_ptr<Expression> lhs,
        Kind kind,
        std::unique_ptr<Expression> rhs)
    : lhs_{std::move(lhs)}, kind_{kind}, rhs_{std::move(rhs)}
{
}

Expression* BinaryOperation::lhs() const
{
    return lhs_.get();
}

BinaryOperation::Kind BinaryOperation::kind() const
{
    return kind_;
}

Expression* BinaryOperation::rhs() const
{
    return rhs_.get();
}

BinaryOperation::Kind BinaryOperation::from_token_kind(const Token::Kind& kind)
{
    switch (kind) {
        case (Token::Kind)'=': return Kind::Assign;
        case (Token::Kind)'+': return Kind::Add;
        case (Token::Kind)'-': return Kind::Sub;
        case (Token::Kind)'*': return Kind::Mul;
        case (Token::Kind)'/': return Kind::Div;
        case (Token::Kind)'<': return Kind::Less;
        case (Token::Kind)'>': return Kind::Greater;
        case Token::EQ: return Kind::Equal;
        case Token::NEQ: return Kind::NotEq;
        case Token::LE: return Kind::LeEq;
        case Token::GE: return Kind::GrEq;
        default: return static_cast<Kind>(-1);
    }
}

const std::map<BinaryOperation::Kind, const std::string>
BinaryOperation::kind_strings {
        { Kind::Assign, "Assign"},
        { Kind::Add, "Add" },
        { Kind::Sub, "Sub" },
        { Kind::Mul, "Mul" },
        { Kind::Div, "Div" },
        { Kind::Less, "Less" },
        { Kind::Greater, "Greater" },
        { Kind::Equal, "Equal" },
        { Kind::NotEq, "NotEq" },
        { Kind::LeEq, "LeEq" },
        { Kind::GrEq, "GrEq" }
};

UnaryOperation::UnaryOperation(Kind kind, std::unique_ptr<Expression> expr)
        : kind_{kind}, expr{std::move(expr)}
{
}

const std::map<UnaryOperation::Kind, const std::string>
UnaryOperation::kind_strings {
        { Kind::Minus, "Minus" },
        { Kind::Not, "Not" }
};

UnaryOperation::Kind UnaryOperation::kind() const
{
    return kind_;
}

Expression* UnaryOperation::expression() const
{
    return expr.get();
}


IfElseExpr::IfElseExpr(
        std::unique_ptr<Expression> condition,
        std::unique_ptr<Expression> if_body,
        std::unique_ptr<Expression> else_body)
    : cond{std::move(condition)},
      if_body_{std::move(if_body)},
      else_body_{std::move(else_body)}
{
}

Expression* IfElseExpr::condition() const
{
    return cond.get();
}

Expression* IfElseExpr::if_body() const
{
    return if_body_.get();
}

Expression* IfElseExpr::else_body() const
{
    return else_body_.get();
}

WhileExpr::WhileExpr(std::unique_ptr<Expression> condition,
                     std::unique_ptr<Expression> body)
        : cond{std::move(condition)}, body_{std::move(body)}
{
}

Expression* WhileExpr::condition() const
{
    return cond.get();
}

Expression* WhileExpr::body() const
{
    return body_.get();
}

FunctionCall::FunctionCall(
        std::string func_name,
        std::vector<std::unique_ptr<Expression>> args)
    : func_name{std::move(func_name)}, args{std::move(args)}
{
}

const std::string& FunctionCall::function_name() const
{
    return func_name;
}

const std::vector<std::unique_ptr<Expression>>&
FunctionCall::arguments() const
{
    return args;
}

Variable::Variable(std::string name)
    : name_{std::move(name)}
{
}

const std::string& Variable::name() const
{
    return name_;
}

BoolValue::BoolValue(bool value)
    : value{value}
{
}

IntValue::IntValue(int value)
    : value{value}
{
}

FloatValue::FloatValue(double value)
    : value{value}
{
}

NullValue::NullValue()
{
}

void Program::accept(Visitor& v) { v.accept(*this); }
void Function::accept(Visitor& v) { v.accept(*this); }
void Parameter::accept(Visitor& v) { v.accept(*this); }
void Block::accept(Visitor& v) { v.accept(*this); }
void Definition::accept(Visitor& v) { v.accept(*this); }
void BinaryOperation::accept(Visitor& v) { v.accept(*this); }
void UnaryOperation::accept(Visitor& v) { v.accept(*this); }
void IfElseExpr::accept(Visitor& v) { v.accept(*this); }
void WhileExpr::accept(Visitor& v) { v.accept(*this); }
void FunctionCall::accept(Visitor& v) { v.accept(*this); }
void Variable::accept(Visitor& v) { v.accept(*this); }
void BoolValue::accept(Visitor& v) { v.accept(*this); }
void IntValue::accept(Visitor& v) { v.accept(*this); }
void FloatValue::accept(Visitor& v) { v.accept(*this); }
void NullValue::accept(Visitor& v) { v.accept(*this); }
void BlankExpr::accept(Visitor& v) { v.accept(*this); }
