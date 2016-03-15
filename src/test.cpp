#include <fstream>
#include <iostream>
#include <memory>
#include <vector>

#include "lex.h"

int main(int argc, char* argv[])
{
    std::ifstream ifs {"/home/semyon/Projects/funlang/example.f"};
    ifs.sync_with_stdio(false);

    Funlang::Lexer lexer{ifs};

    std::unique_ptr<Funlang::Token> token;
    size_t line = lexer.line();
    while (token = lexer.get_token())
    {
        if (lexer.line() > line)
        {
            for (line; line < lexer.line(); ++line)
                std::cout << '\n';
        }
        std::cout << *token << ' ';
    }
}
