#include <algorithm>
#include <fstream>
#include <iostream>
#include <memory>
#include <vector>

#include "llvm/IR/Value.h"

#include "ast_printer.h"
#include "codegen.h"
#include "lex.h"
#include "parse.h"

int main(int argc, char* argv[])
{
    std::cout << "---------------- Lexemes ----------------\n";
    {
        std::ifstream ifs{argv[1]};
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

    std::cout << "\n\n---------------- AST ----------------\n";
    std::ifstream ifs {argv[1]};
    Funlang::Lexer lexer{ifs};
    Funlang::Parser parser{lexer};
    auto program = parser.parse_all();
    Funlang::AST::print(*program);

    std::cout << "\n\n--------------- LLVM IR -----------------\n";
    Funlang::AST::codegen(*program);
}
