#include <algorithm>
#include <fstream>
#include <iostream>
#include <memory>
#include <vector>
#include <parse.h>

#include "ast_printer.h"
#include "lex.h"

int main(int argc, char* argv[])
{
    std::cout << "---------------- Lexemes ----------------\n";
    {
        std::ifstream ifs{"/home/semyon/Projects/funlang/examples/factorial.ff"};
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

    std::cout << "\n\n";
    std::cout << "---------------- AST ----------------\n";
    {
        std::ifstream ifs {"/home/semyon/Projects/funlang/examples/factorial.ff"};
        Funlang::Lexer lexer{ifs};
        Funlang::Parser parser{lexer};
        Funlang::AST::ProgramAST program_tree = parser.parse_all();
        Funlang::AST::Printer::print(*program_tree.root());
    }
}
