#include <algorithm>
#include <memory>
#include <stack>
#include <vector>

#include "parse.h"

using namespace Funlang;
using namespace Funlang::AST;

Parser::Parser(Lexer& lexer)
    : lexer{lexer},
      tokens{lexer.get_tokens()},
      current_token{std::cbegin(tokens)}
{

}

std::unique_ptr<Token> Parser::consume_if(Token::Kind expected_kind)
{
    if (current_token == std::cend(tokens)) {
        throw NoMoreTokens{};
    }
    if (expected_kind != (*current_token)->kind()) {
        throw ParseError{expected_kind, **current_token,
                         lexer.line(), lexer.column()};
    }
    return *current_token++;
}

std::unique_ptr<Token> Parser::consume_if(char expected_kind)
{
    return consume_if(static_cast<Token::Kind>(expected_kind));
}

std::vector<Parameter> Parser::optparams()
{
    // todo
    auto param_id_token = consume_if(Token::Kind::ID);
    if (!param_id_token)
        return std::vector<Parameter>{};

    Id* param_id = dynamic_cast<Id*>(param_id_token.get());
    std::string param_name = param_id->name;

    consume_if(':');

    auto param_typeid_token = consume_if(Token::Kind::ID);
    Id* param_typeid = dynamic_cast<Id*>(param_typeid_token.get());
    std::string param_typename = param_typeid->name;

    //todo
    //auto rest = params_rest();
    //rest.push_back()
}

std::unique_ptr<Function> Parser::function()
{
    auto func_first = consume_if(Token::Kind::FUNCTION);
    if (!func_first) return nullptr;

    auto func_id_token = consume_if(Token::Kind::ID);
    Id* func_id = dynamic_cast<Id*>(func_id_token.get());
    std::string func_name = func_id->name;

    consume_if('(');
    auto optparams = optparams();
    consume_if(')');
    consume_if(':');

    auto ret_type_token = consume_if(Token::Kind::ID);
    Id* ret_type_id = dynamic_cast<Id*>(ret_type_token.get());
    std::string ret_type = ret_type_id->name;

    auto body = block();

    return std::make_unique<Function>(func_name, optparams, ret_type, body);
}

std::vector<std::unique_ptr<Function>> Parser::functions()
{
    auto func = function();
    if (func == nullptr)
        return std::vector<std::unique_ptr<Function>>{};

    auto optfuncs = functions();
    optfuncs.push_back(std::move(func));
    return optfuncs;
}


std::unique_ptr<Program> Parser::program()
{
    auto reversed_funcs = functions();
    std::reverse(std::begin(reversed_funcs), std::end(reversed_funcs));
    return std::make_unique<Function>(std::move(reversed_funcs));
}

ProgramAST Parser::parse_all()
{
    auto token_iter = std::cbegin(tokens);
    program();
}
