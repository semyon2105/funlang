#include "ast_nodes.h"

using namespace Funlang::AST;

Node::~Node()
{

}

Program::Program()
    : functions_{std::vector<Function*>{}}, entrypoint_{nullptr}
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
