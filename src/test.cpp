#include <algorithm>
#include <fstream>
#include <iostream>
#include <memory>
#include <vector>

#include "lex.h"

int main(int argc, char* argv[])
{
    {
        std::ifstream ifs {"/home/semyon/Projects/funlang/example.f"};
        ifs.sync_with_stdio(false);
        Funlang::Lexer lexer{ifs};

        size_t line = lexer.line();
        std::unique_ptr<Funlang::Token> token;
        while (token = lexer.get_token()) {
            if (lexer.line() > line) {
                for (line; line < lexer.line(); ++line)
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

        auto tokens = lexer.get_tokens();
        std::for_each(std::cbegin(tokens), std::cend(tokens), [](const auto& token) { std::cout << *token << ' '; });
    }
}
