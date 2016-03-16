#ifndef FUNLANG_PARSE_H
#define FUNLANG_PARSE_H

#include "ast_nodes.h"
#include "lex.h"
#include "program_ast.h"

namespace Funlang
{

struct NoMoreTokens {};

struct ParseError
{
    ParseError(Token::Kind expected, Token& got, size_t lineno, size_t colno)
            : message{"Expected token of kind " +
                      Token::kind_to_string(expected) +
                      " , got: " + got.to_string()}
    {
    }

    const std::string message;
};

class Parser
{
public:
    Parser(Lexer& lexer);

    AST::ProgramAST parse_all();

private:
    Lexer& lexer;
    const std::vector<std::unique_ptr<Token>> tokens;
    std::vector::const_iterator current_token;

    std::unique_ptr<Token> consume_if(char kind);
    std::unique_ptr<Token> consume_if(Token::Kind kind);

    auto program()
        -> std::unique_ptr<AST::Program>;

    auto functions()
        -> std::vector<std::unique_ptr<AST::Function>>;

    auto function()
        -> std::unique_ptr<AST::Function>;

    auto optparams()
        -> std::vector<AST::Parameter>;

    auto params_rest()
        -> std::vector<AST::Parameter>;

    auto block()
        -> std::unique_ptr<AST::Block>;
};

}
#endif //FUNLANG_PARSE_H
