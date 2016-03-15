#include "program_ast.h"

using namespace Funlang::AST;

ProgramAST::ProgramAST(std::unique_ptr<Program> root)
    : root_{std::move(root)}
{
}

