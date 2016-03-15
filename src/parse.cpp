#include <memory>
#include <stack>

#include "parse.h"

using namespace Funlang;
using namespace Funlang::AST;

Parser::Parser(Lexer& lexer)
    : lexer{lexer}, tokens{lexer.get_tokens()}
{

}

ProgramAST Parser::parse_all()
{
    auto token_iter = std::cbegin(tokens);
}
