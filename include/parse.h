#ifndef FUNLANG_PARSE_H
#define FUNLANG_PARSE_H

#include "ast_nodes.h"
#include "lex.h"
#include "program_ast.h"

namespace Funlang
{

class Parser
{
public:
    Parser(Lexer& lexer);

    AST::ProgramAST parse_all();

private:
    Lexer& lexer;
    const std::vector<std::unique_ptr<Token>> tokens;
};

}
#endif //FUNLANG_PARSE_H
