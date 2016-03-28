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
        std::ifstream ifs{"/home/semyon/Projects/funlang/examples/fib.ff"};
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
    std::ifstream ifs {"/home/semyon/Projects/funlang/examples/fib.ff"};
    Funlang::Lexer lexer{ifs};
    Funlang::Parser parser{lexer};
    Funlang::AST::ProgramAST program_tree = parser.parse_all();
    Funlang::AST::Printer::print(*program_tree.root());
    std::cout << "\n\n--------------- IR -----------------\n";
    llvm::Value* code = Funlang::AST::codegen(program_tree);
    code->dump();
}
