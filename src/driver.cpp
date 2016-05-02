#include <fstream>
#include <iostream>

#include "codegen.h"
#include "lex.h"
#include "parse.h"

int main(int argc, char* argv[])
{
    if (argc != 2) {
        std::cout << "Usage: funlang [source_file_path]\n";
        return 0;
    }

    std::string input_path = argv[1];
    size_t name_pos = input_path.find_last_of('/');
    if (name_pos == std::string::npos) {
        name_pos = 0;
    }
    size_t ext_pos = input_path.rfind(".ff");
    std::string output_filename =
            input_path.substr(name_pos + 1, ext_pos - name_pos - 1) + ".ll";

    std::ifstream input_file { input_path };
    std::ofstream output_file { output_filename };

    Funlang::Lexer lexer { input_file };
    Funlang::Parser parser { lexer };
    output_file << Funlang::AST::codegen(*parser.parse_all());
}
