#include "ast_nodes.h"

using namespace Funlang::AST;

Node::~Node()
{

}

Program::Program(std::vector<std::unique_ptr<Function>> functions)
    : functions_{functions}
{

}

Node::Node()
{

}

Expression::Expression()
{

}

const Type& Definition::type() const
{
    return Type::Error;
}

const Type& Assignment::type() const
{
    return Type::Error;
}
