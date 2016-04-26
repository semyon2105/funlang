#include <algorithm>
#include <memory>
#include <stack>

#include "parse.h"
#include "util.h"

using namespace Funlang;
using namespace Funlang::AST;

ParseError::ParseError(Token at, size_t lineno)
    : at{at}, lineno{lineno}
{
}

UnexpectedTokenError::UnexpectedTokenError(
        Token::Kind expected,
        Token at,
        size_t lineno)
    : ParseError{at, lineno}, expected{expected}
{
}

LValueExpected::LValueExpected(Token at, size_t lineno)
    : ParseError{at, lineno}
{
}

Parser::Parser(Lexer& lexer)
    : lexer{lexer},
      lookahead_buffer(16),
      current_token{lookahead(0)}
{
}

Token* Parser::lookahead(size_t offset)
{
    size_t buf_size = lookahead_buffer.size();
    if (offset < buf_size) {
        return lookahead_buffer[offset].get();
    }
    for (size_t pos = buf_size; pos <= offset; ++pos) {
        if (lexer.look_ahead() == '\0') {
            eof_token = std::make_unique<Token>('\0');
            return eof_token.get();
        }
        lookahead_buffer.push_back(lexer.get_token());
    }
    return lookahead_buffer.back().get();
}

std::unique_ptr<Token> Parser::consume(Token::Kind expected_kind)
{
    if (!current_token) {
        throw ParseError{Token{'\0'}, lexer.line()};
    }
    if (expected_kind != current_token->kind) {
        throw UnexpectedTokenError{expected_kind, *current_token,
                         lexer.line()};
    }
    auto consumed_token = std::move(*std::begin(lookahead_buffer));
    lookahead_buffer.pop_front();
    current_token = lookahead(0);
    return consumed_token;
}

std::unique_ptr<Token> Parser::consume(char expected_kind)
{
    return consume(static_cast<Token::Kind>(expected_kind));
}

std::unique_ptr<LValue> Parser::lvalue()
{
    Token::Kind token_after_id = lookahead(1)->kind;
    if (current_token->kind == Token::ID && token_after_id == '[') {
        auto id_token = consume(Token::ID);
        Id* id = dynamic_cast<Id*>(id_token.get());

        std::vector<std::unique_ptr<Expression>> index_exprs;
        while (current_token->kind == '[') {
            consume('[');
            index_exprs.emplace_back(expression());
            consume(']');
        }

        return std::make_unique<ArrayAccess>(id->name, std::move(index_exprs));
    }
    if (current_token->kind == Token::ID) {
        auto id_token = consume(Token::ID);
        Id* id = dynamic_cast<Id*>(id_token.get());
        return std::make_unique<Variable>(id->name);
    }
    return nullptr;
}

std::unique_ptr<Literal> Parser::literal()
{
    if (current_token->kind == Token::INT) {
        auto integer_token = consume(Token::INT);
        Int* integer = dynamic_cast<Int*>(integer_token.get());
        return std::make_unique<IntValue>(integer->value);
    }
    if (current_token->kind == Token::FLOAT) {
        auto float_token = consume(Token::FLOAT);
        Float* fp_number = dynamic_cast<Float*>(float_token.get());
        return std::make_unique<FloatValue>(fp_number->value);
    }
    if (current_token->kind == Token::TRUE) {
        consume(Token::TRUE);
        return std::make_unique<BoolValue>(true);
    }
    if (current_token->kind == Token::FALSE) {
        consume(Token::FALSE);
        return std::make_unique<BoolValue>(false);
    }
    if (current_token->kind == Token::NULLVAL) {
        consume(Token::NULLVAL);
        return std::make_unique<NullValue>();
    }
    return nullptr;
}

std::unique_ptr<ArrayExpr> Parser::array()
{
    if (current_token->kind == '[') {
        consume('[');
        std::vector<std::unique_ptr<Expression>> elements;
        while (auto expr = expression()) {
            elements.emplace_back(std::move(expr));
            if (current_token->kind != ',') {
                break;
            }
            consume(',');
        }
        consume(']');
        return std::make_unique<ArrayExpr>(std::move(elements));
    }
    return nullptr;
}

std::unique_ptr<FunctionCall> Parser::function_call()
{
    if (current_token->kind != Token::ID || lookahead(1)->kind != '(') {
        return nullptr;
    }
    auto func_id_token = consume(Token::ID);
    Id* func_id = dynamic_cast<Id*>(func_id_token.get());

    consume('(');
    auto args = optargs();
    consume(')');

    return std::make_unique<FunctionCall>(func_id->name, std::move(args));
}

std::unique_ptr<WhileExpr> Parser::while_expr()
{
    if (current_token->kind != Token::WHILE) {
        return nullptr;
    }
    consume(Token::WHILE);

    auto condition = expression();

    auto body = block();

    return std::make_unique<WhileExpr>(std::move(condition), std::move(body));
}

std::unique_ptr<Expression> Parser::else_tail()
{
    auto if_else = if_else_expr();
    if (if_else) {
        return if_else;
    }
    return block();
}

std::unique_ptr<Expression> Parser::if_else_expr()
{
    if (current_token->kind != Token::IF) {
        return nullptr;
    }

    consume(Token::IF);
    auto condition = expression();

    auto if_body = block();

    if (current_token->kind != Token::ELSE) {
        return std::make_unique<IfElseExpr>(std::move(condition), std::move(if_body));
    }

    consume(Token::ELSE);
    auto else_body = else_tail();

    return std::make_unique<IfElseExpr>(
                std::move(condition),
                std::move(if_body),
                std::move(else_body));
}

std::unique_ptr<Definition> Parser::definition()
{
    if (current_token->kind != Token::LET) {
        return nullptr;
    }
    consume(Token::LET);

    auto id_token = consume(Token::ID);
    Id* id = dynamic_cast<Id*>(id_token.get());

    std::unique_ptr<StaticTypeId> type = nullptr;
    if (current_token->kind == ':') {
        consume(':');
        type = static_type();
    }

    consume('=');

    auto rhs = expression();

    return std::make_unique<Definition>(
            std::move(id->name),
            type ? std::move(type) : std::make_unique<EmptyTypeId>(),
            std::move(rhs)
    );
}

std::unique_ptr<Expression> Parser::primary()
{
    if (current_token->kind == '-') {
        consume('-');
        return std::make_unique<UnaryOperation>(
                    UnaryOperation::Kind::Minus, primary());
    }
    if (current_token->kind == '!') {
        consume('!');
        return std::make_unique<UnaryOperation>(
                    UnaryOperation::Kind::Not, primary());
    }
    if (current_token->kind == '(') {
        consume('(');
        auto expr = expression();
        consume(')');
        return expr;
    }
    std::unique_ptr<Expression> prim;
    if ((prim = definition()) ||
        (prim = if_else_expr()) ||
        (prim = while_expr()) ||
        (prim = function_call()) ||
        (prim = array()) ||
        (prim = literal()) ||
        (prim = lvalue())) {
        return prim;
    }
    return nullptr;
}

std::unique_ptr<Expression> Parser::muldiv()
{
    auto lhs = primary();

    Token::Kind muldiv_op = current_token->kind;
    if (muldiv_op == '*' || muldiv_op == '/') {
        auto op_token = consume(muldiv_op);
        auto kind = BinaryOperation::from_token_kind(op_token->kind);
        auto rhs = muldiv();
        return std::make_unique<BinaryOperation>(
                std::move(lhs), kind, std::move(rhs)
        );
    }

    return lhs;
}

std::unique_ptr<Expression> Parser::addsub()
{
    auto lhs = muldiv();

    Token::Kind addsub_op = current_token->kind;
    if (addsub_op == '+' || addsub_op == '-') {
        auto op_token = consume(addsub_op);
        auto kind = BinaryOperation::from_token_kind(op_token->kind);
        auto rhs = addsub();
        return std::make_unique<BinaryOperation>(
                std::move(lhs), kind, std::move(rhs)
        );
    }

    return lhs;
}

std::unique_ptr<Expression> Parser::conditional()
{
    auto lhs = addsub();

    Token::Kind cond_op = current_token->kind;
    if (cond_op == '<' || cond_op == '>' ||
        cond_op == Token::EQ || cond_op == Token::NEQ ||
        cond_op == Token::LE || cond_op == Token::GE)
    {
        auto op_token = consume(cond_op);
        auto kind = BinaryOperation::from_token_kind(op_token->kind);
        auto rhs = conditional();
        return std::make_unique<BinaryOperation>(
                    std::move(lhs), kind, std::move(rhs)
        );
    }

    return lhs;
}

std::vector<std::unique_ptr<Expression>> Parser::optargs()
{
    std::vector<std::unique_ptr<Expression>> args;
    std::unique_ptr<Expression> argument;
    while ((argument = expression())) {
        args.push_back(std::move(argument));

        if (current_token->kind != ',') {
            break;
        }
        consume(',');
    }
    return args;
}

std::unique_ptr<Expression> Parser::assignment()
{
    auto lhs = conditional();
    if (auto lvalue = dynamic_cast<LValue*>(lhs.get())
        && current_token->kind == '=')
    {
        auto op_token = consume('=');
        auto kind = BinaryOperation::from_token_kind(op_token->kind);
        auto rhs = assignment();
        return std::make_unique<BinaryOperation>(
                    std::move(lhs), kind, std::move(rhs));
    }
    return lhs;
}

std::unique_ptr<Expression> Parser::expression()
{
    std::unique_ptr<Expression> expr;
    if ((expr = block()) || (expr = assignment())) {
        return expr;
    }
    return nullptr;
}

std::vector<std::unique_ptr<Expression>> Parser::optexprs()
{
    std::vector<std::unique_ptr<Expression>> exprs;
    std::unique_ptr<Expression> expr = expression();

    if (!expr) {
        exprs.push_back(std::make_unique<BlankExpr>());
        return exprs;
    }

    while (true) {
        exprs.push_back(std::move(expr));

        if (current_token->kind != ';') {
            break;
        }
        consume(';');

        expr = expression();

        if (!expr) {
            exprs.push_back(std::make_unique<BlankExpr>());
            break;
        }
    }

    return exprs;
}

std::unique_ptr<StaticTypeId> Parser::static_type()
{
    if (current_token->kind == Token::ID) {
        auto id_token = consume(Token::ID);
        Id* id = dynamic_cast<Id*>(id_token.get());
        if (current_token->kind != '[') {
            return std::make_unique<PrimitiveTypeId>(id->name);
        }

        std::vector<int> dim_sizes;
        while (current_token->kind == '[') {
            consume('[');
            auto integer_token = consume(Token::INT);
            Int* integer = dynamic_cast<Int*>(integer_token.get());
            consume(']');
            dim_sizes.push_back(integer->value);
        }
        return std::make_unique<ArrayTypeId>(id->name, dim_sizes);
    }
    return nullptr;
}

std::unique_ptr<Block> Parser::block()
{
    if (current_token->kind == '{') {
        consume('{');
        auto exprs = optexprs();
        consume('}');
        return std::make_unique<Block>(std::move(exprs));
    }
    return nullptr;
}

std::unique_ptr<Parameter> Parser::parameter(bool is_optional)
{
    if (is_optional && current_token->kind != Token::ID) {
        return nullptr;
    }
    auto param_id_token = consume(Token::ID);
    Id* param_id = dynamic_cast<Id*>(param_id_token.get());
    const std::string& param_name = param_id->name;

    consume(':');

    auto type = static_type();

    return std::make_unique<Parameter>(param_name, std::move(type));
}

std::vector<std::unique_ptr<Parameter>> Parser::optparams()
{
    std::vector<std::unique_ptr<Parameter>> params;
    std::unique_ptr<Parameter> param;
    if ((param = parameter(true))) {
        while (true) {
            params.push_back(std::move(param));

            if (current_token->kind != ',') {
                break;
            }
            consume(',');

            param = parameter(false);
        }
    }
    return params;
}

std::unique_ptr<Function> Parser::function()
{
    if (!current_token) {
        return nullptr;
    }

    if (current_token->kind != Token::FUNCTION) {
        if (current_token->kind == '\0') {
            return nullptr;
        }
        throw UnexpectedTokenError{
                Token::FUNCTION, *current_token, lexer.line()
        };
    }

    auto func_keyword = consume(Token::FUNCTION);

    auto func_id_token = consume(Token::ID);
    Id* func_id = dynamic_cast<Id*>(func_id_token.get());

    consume('(');
    auto params = optparams();
    consume(')');
    consume(':');

    auto type = static_type();

    auto body = block();

    return std::make_unique<Function>(
            func_id->name,
            std::move(params),
            std::move(type),
            std::move(body)
    );
}

std::vector<std::unique_ptr<Function>> Parser::functions()
{
    std::vector<std::unique_ptr<Function>> funcs;
    std::unique_ptr<Function> func;
    while ((func = function())) {
        funcs.push_back(std::move(func));
    }
    return funcs;
}


std::unique_ptr<Program> Parser::program()
{
    return std::make_unique<Program>(functions());
}

std::unique_ptr<Program> Parser::parse_all()
{
    return program();
}
