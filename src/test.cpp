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
    if (argc != 2) {
        throw std::runtime_error{"Must have 1 argument"};
    }
    std::cout << "---------------- Lexemes ----------------\n";
    {
        std::ifstream ifs{argv[1]};
        Funlang::Lexer lexer{ifs};
        if (!ifs) {
            throw std::runtime_error{std::string{"Unable to open file "} + argv[1]};
        }

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
    if (!ifs) {
        throw std::runtime_error{std::string{"Unable to open file "} + argv[1]};
    }
    Funlang::Parser parser{lexer};
    auto program = parser.parse_all();
    Funlang::AST::print(*program);

    std::cout << "\n\n--------------- LLVM IR -----------------\n";
    std::string code = Funlang::AST::codegen(*program);
    std::cout << code;
    std::ofstream ofs {"out.ll"};
    ofs << code;
}
