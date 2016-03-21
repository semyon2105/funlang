#include <boost/core/demangle.hpp>

#include "ast_printer.h"

using namespace Funlang;
using namespace Funlang::AST;
using namespace Funlang::AST::impl;

template <typename T>
std::string type_name(const T& t)
{
    return boost::core::demangle(typeid(t).name());
}

void Printer::print(Node& tree)
{
    print(std::cout, tree);
}

void Printer::print(std::ostream& os, Node& tree)
{
    auto printer_visitor = PrinterVisitor{os};
    tree.accept(printer_visitor);
}

PrinterVisitor::PrinterVisitor(std::ostream& os)
    : os{os}
{
}

struct PrinterVisitor::LevelGuard
{
    LevelGuard(size_t& level) : level{level} { ++this->level; }
    ~LevelGuard() { --level; }

    size_t& level;
};

void PrinterVisitor::print(const std::string& s)
{
    for (size_t i = 0; i < level; ++i) {
        os << "|   ";
    }
    os << s << '\n';
}

void PrinterVisitor::accept(Program& p)
{
    print(type_name(p));
    auto level_guard = LevelGuard{level};
    for (const auto& f : p.functions()) {
        f->accept(*this);
    }
}

void PrinterVisitor::accept(Function& f)
{
    print(std::string{type_name(f)});
    auto level_guard = LevelGuard{level};
    print("name=" + f.name() + ", return_type=" + f.return_type());
    print("[Parameters]");
    if (f.parameters().empty()) {
        print("<Nothing>");
    }
    for (const auto& p: f.parameters()) {
        p->accept(*this);
    }
    print("[Body]");
    f.body()->accept(*this);
}

void PrinterVisitor::accept(Parameter& p)
{
    print(std::string{type_name(p)});
    auto level_guard = LevelGuard{level};
    print(p.name() + " : " + p.type_name());
}

void PrinterVisitor::accept(Block& b)
{
    print(std::string{type_name(b)});
    auto level_guard = LevelGuard{level};
    for (const auto& e : b.expressions()) {
        e->accept(*this);
    }
}

void PrinterVisitor::accept(Definition& d)
{
    print(std::string{type_name(d)});
    auto level_guard = LevelGuard{level};
    print(std::string{"name="} + d.var_name() +
          ", static_type=" + d.var_type());
    print("[Definition_RHS]");
    d.expression()->accept(*this);
}
void PrinterVisitor::accept(Assignment& a)
{
    print(std::string{type_name(a)});
    auto level_guard = LevelGuard{level};
    print(std::string{"name="} + a.variable_name());
    print("[Assignment_RHS]");
    a.expression()->accept(*this);
}
void PrinterVisitor::accept(BinaryOperation& b)
{
    print(std::string{type_name(b)});
    auto level_guard = LevelGuard{level};
    print("kind=" + BinaryOperation::kind_strings.at(b.kind()));
    print("[LHS]");
    b.lhs()->accept(*this);
    print("[RHS]");
    b.rhs()->accept(*this);
}
void PrinterVisitor::accept(UnaryOperation& u)
{
    print(std::string{type_name(u)});
    auto level_guard = LevelGuard{level};
    print(std::string{"kind="} + u.kind_strings.at(u.kind()));
    print("[Operand]");
    u.expression()->accept(*this);
}
void PrinterVisitor::accept(IfExpr& i)
{
    print(std::string{type_name(i)});
    auto level_guard = LevelGuard{level};
    print("[Condition]");
    i.condition()->accept(*this);
    print("[Body]");
    i.body()->accept(*this);
}
void PrinterVisitor::accept(WhileExpr& w)
{
    print(std::string{type_name(w)});
    auto level_guard = LevelGuard{level};
    print("[Condition]");
    w.condition()->accept(*this);
    print("[Body]");
    w.body()->accept(*this);
}
void PrinterVisitor::accept(FunctionCall& f)
{
    print(std::string{type_name(f)});
    auto level_guard = LevelGuard{level};
    print(std::string{"name="} + f.function_name());
    print("[Arguments]");
    for (const auto& arg : f.arguments()) {
        arg->accept(*this);
    }
}
void PrinterVisitor::accept(Variable& v)
{
    print(std::string{type_name(v)});
    auto level_guard = LevelGuard{level};
    print("name=" + v.name());
}
void PrinterVisitor::accept(BoolValue& v)
{
    print(std::string{type_name(v)});
    auto level_guard = LevelGuard{level};
    print("value=" + std::to_string(v.value));
}
void PrinterVisitor::accept(IntValue& v)
{
    print(std::string{type_name(v)});
    auto level_guard = LevelGuard{level};
    print("value=" + std::to_string(v.value));
}
void PrinterVisitor::accept(FloatValue& v)
{
    print(std::string{type_name(v)});
    auto level_guard = LevelGuard{level};
    print("value=" + std::to_string(v.value));
}
void PrinterVisitor::accept(NullValue& v)
{
    print(std::string{type_name(v)});
}
void PrinterVisitor::accept(BlankExpr& b)
{
    print(std::string{type_name(b)});
}
