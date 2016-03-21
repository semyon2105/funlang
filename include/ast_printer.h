#ifndef FUNLANG_AST_PRINTER_H
#define FUNLANG_AST_PRINTER_H

#include "ast_nodes.h"

namespace Funlang
{

namespace AST
{

class Printer
{
public:
    static void print(Node& tree);
    static void print(std::ostream& os, Node& tree);
};

namespace impl
{

class PrinterVisitor : public Visitor
{
public:
    PrinterVisitor(std::ostream& os = std::cout);

    void accept(Program&) override;
    void accept(Function&) override;
    void accept(Parameter&) override;
    void accept(Block&) override;
    void accept(Definition&) override;
    void accept(Assignment&) override;
    void accept(BinaryOperation&) override;
    void accept(UnaryOperation&) override;
    void accept(IfExpr&) override;
    void accept(WhileExpr&) override;
    void accept(FunctionCall&) override;
    void accept(Variable&) override;
    void accept(BoolValue&) override;
    void accept(IntValue&) override;
    void accept(FloatValue&) override;
    void accept(NullValue&) override;
    void accept(BlankExpr&) override;

private:
    std::ostream& os;
    size_t level = 0;

    struct LevelGuard;

    void print(const std::string& s);
};

}

}

}

#endif //FUNLANG_AST_PRINTER_H
