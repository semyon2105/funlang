#include <algorithm>
#include <fstream>
#include <iostream>
#include <memory>
#include <vector>
#include <parse.h>

#include "lex.h"

int main(int argc, char* argv[])
{
    {
        std::ifstream ifs {"/home/semyon/Projects/funlang/example.f"};
        ifs.sync_with_stdio(false);
        Funlang::Lexer lexer{ifs};

        size_t line = lexer.line();
        std::unique_ptr<Funlang::Token> token;
        while ((token = lexer.get_token())) {
            if (lexer.line() > line) {
                for (; line < lexer.line(); ++line)
                    std::cout << '\n';
            }
            std::cout << *token << ' ';
        }
    }

    std::cout << std::endl;

    {
        std::ifstream ifs {"/home/semyon/Projects/funlang/example.f"};
        ifs.sync_with_stdio(false);
        Funlang::Lexer lexer{ifs};
        Funlang::Parser parser{lexer};
        Funlang::AST::ProgramAST program_tree = parser.parse_all();
    }
}
