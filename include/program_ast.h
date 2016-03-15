#ifndef FUNLANG_PROGRAM_AST_H
#define FUNLANG_PROGRAM_AST_H

#include <memory>
#include <vector>

#include "ast_nodes.h"

namespace Funlang
{

namespace AST
{

class ProgramAST
{
public:
    ProgramAST(std::unique_ptr<Program>);

    Program* root() const;

private:
    std::unique_ptr<Program> root_;
};

}

}

#endif //FUNLANG_PROGRAM_AST_H
