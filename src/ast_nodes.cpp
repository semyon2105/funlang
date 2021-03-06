#include <utility>
#include <parse.h>

#include "ast_nodes.h"

using namespace Funlang::AST;

Program::Program(std::vector<std::unique_ptr<Function>> functions)
    : functions{std::move(functions)}
{
}

Function::Function(std::string name,
                   std::vector<std::unique_ptr<Parameter>> params,
                   std::unique_ptr<StaticTypeId> return_type,
                   std::unique_ptr<Expression> body)
    : name{std::move(name)},
      params{std::move(params)},
      return_type{std::move(return_type)},
      body{std::move(body)}
{
}

Parameter::Parameter(std::string name, std::unique_ptr<StaticTypeId> type)
    : name{std::move(name)}, type{std::move(type)}
{
}

Block::Block(std::vector<std::unique_ptr<Expression>> exprs)
    : exprs{std::move(exprs)}
{
}

StaticTypeId::StaticTypeId(std::string name)
    : name{std::move(name)}
{
}

PrimitiveTypeId::PrimitiveTypeId(std::string name)
    : StaticTypeId{std::move(name)}
{
}

std::string array_type_name(const std::string& primitive_type_name,
                            const std::vector<int>& dim_sizes)
{
    std::string type_name = primitive_type_name;
    for (int dim_size : dim_sizes) {
        type_name += "[" + std::to_string(dim_size) + "]";
    }
    return type_name;
}

ArrayTypeId::ArrayTypeId(std::string primitive_type_name,
                         std::vector<int> dim_sizes)
    : StaticTypeId{array_type_name(primitive_type_name, dim_sizes)},
      primitive_type{std::make_unique<PrimitiveTypeId>(
                         std::move(primitive_type_name))},
      dim_sizes{std::move(dim_sizes)}
{
}

Definition::Definition(
        std::string name,
        std::unique_ptr<StaticTypeId> type,
        std::unique_ptr<Expression> rhs)
    : name{std::move(name)}, type{std::move(type)}, rhs{std::move(rhs)}
{
}

BinaryOperation::BinaryOperation(
        std::unique_ptr<Expression> lhs,
        Kind kind,
        std::unique_ptr<Expression> rhs)
    : lhs{std::move(lhs)}, kind{kind}, rhs{std::move(rhs)}
{
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
        case Token::AND: return Kind::And;
        case Token::OR: return Kind::Or;
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
        { Kind::GrEq, "GrEq" },
        { Kind::And, "And" },
        { Kind::Or, "Or" }
};

UnaryOperation::UnaryOperation(Kind kind, std::unique_ptr<Expression> expr)
        : kind{kind}, expr{std::move(expr)}
{
}

const std::map<UnaryOperation::Kind, const std::string>
UnaryOperation::kind_strings {
        { Kind::Minus, "Minus" },
        { Kind::Not, "Not" }
};

IfElseExpr::IfElseExpr(
        std::unique_ptr<Expression> condition,
        std::unique_ptr<Expression> if_body,
        std::unique_ptr<Expression> else_body)
    : condition{std::move(condition)},
      if_body{std::move(if_body)},
      else_body{std::move(else_body)}
{
}

WhileExpr::WhileExpr(std::unique_ptr<Expression> condition,
                     std::unique_ptr<Expression> body)
        : condition{std::move(condition)}, body{std::move(body)}
{
}

FunctionCall::FunctionCall(
        std::string func_name,
        std::vector<std::unique_ptr<Expression>> args)
    : callee_name{std::move(func_name)}, args{std::move(args)}
{
}

ArrayExpr::ArrayExpr(std::vector<std::unique_ptr<Expression>> elements)
    : elements{std::move(elements)}
{
}

Variable::Variable(std::string name)
    : name{std::move(name)}
{
}

ArrayAccess::ArrayAccess(std::string name,
                         std::vector<std::unique_ptr<Expression>> index_exprs)
    : name{std::move(name)}, index_exprs{std::move(index_exprs)}
{
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

void Program::accept(Visitor& v) { v.accept(*this); }
void Function::accept(Visitor& v) { v.accept(*this); }
void Parameter::accept(Visitor& v) { v.accept(*this); }
void Block::accept(Visitor& v) { v.accept(*this); }
void StaticTypeId::accept(Visitor& v) { v.accept(*this); }
void PrimitiveTypeId::accept(Visitor& v) { v.accept(*this); }
void ArrayTypeId::accept(Visitor& v) { v.accept(*this); }
void Definition::accept(Visitor& v) { v.accept(*this); }
void BinaryOperation::accept(Visitor& v) { v.accept(*this); }
void UnaryOperation::accept(Visitor& v) { v.accept(*this); }
void IfElseExpr::accept(Visitor& v) { v.accept(*this); }
void WhileExpr::accept(Visitor& v) { v.accept(*this); }
void FunctionCall::accept(Visitor& v) { v.accept(*this); }
void Variable::accept(Visitor& v) { v.accept(*this); }
void ArrayAccess::accept(Visitor& v) { v.accept(*this); }
void BoolValue::accept(Visitor& v) { v.accept(*this); }
void IntValue::accept(Visitor& v) { v.accept(*this); }
void FloatValue::accept(Visitor& v) { v.accept(*this); }
void ArrayExpr::accept(Visitor& v) { v.accept(*this); }
void NullValue::accept(Visitor& v) { v.accept(*this); }
void BlankExpr::accept(Visitor& v) { v.accept(*this); }
