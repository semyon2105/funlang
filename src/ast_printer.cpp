#include <boost/core/demangle.hpp>

#include "ast_printer.h"

using namespace Funlang;
using namespace Funlang::AST;
using namespace Funlang::AST::Codegen;

template <typename T>
std::string type_name(const T& t)
{
    return boost::core::demangle(typeid(t).name());
}

void AST::print(Node& tree)
{
    print(std::cout, tree);
}

void AST::print(std::ostream& os, Node& tree)
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
    for (const auto& f : p.functions) {
        f->accept(*this);
    }
}

void PrinterVisitor::accept(Function& f)
{
    print(std::string{type_name(f)});
    auto level_guard = LevelGuard{level};
    print("name=" + f.name);
    print("[Parameters]");
    if (f.params.empty()) {
        print("<Nothing>");
    }
    for (const auto& p: f.params) {
        p->accept(*this);
    }
    print("[Body]");
    f.body->accept(*this);
    print("[ReturnType]");
    f.return_type->accept(*this);
}

void PrinterVisitor::accept(Parameter& p)
{
    print(std::string{type_name(p)});
    auto level_guard = LevelGuard{level};
    print("name=" + p.name);
    print("[Type]");
    p.type->accept(*this);
}

void PrinterVisitor::accept(Block& b)
{
    print(std::string{type_name(b)});
    auto level_guard = LevelGuard{level};
    for (const auto& e : b.exprs) {
        e->accept(*this);
    }
}

void PrinterVisitor::accept(StaticTypeId& t)
{
    print(std::string{type_name(t)});
    auto level_guard = LevelGuard{level};
    print("name=" + t.name);
}

void PrinterVisitor::accept(PrimitiveTypeId& t)
{
    print(std::string{type_name(t)});
    auto level_guard = LevelGuard{level};
    print("name=" + t.name);
}

void PrinterVisitor::accept(ArrayTypeId& t)
{
    print(std::string{type_name(t)});
    auto level_guard = LevelGuard{level};
    std::string dim_sizes;
    for (int size : t.dim_sizes) {
        dim_sizes += "[" + std::to_string(size) + "]";
    }
    print("name=" + t.name + dim_sizes);
}

void PrinterVisitor::accept(EmptyTypeId& t)
{
    print(std::string{type_name(t)});
}

void PrinterVisitor::accept(Definition& d)
{
    print(std::string{type_name(d)});
    auto level_guard = LevelGuard{level};
    print("name=" + d.name);
    print("[Type]");
    d.type->accept(*this);
    print("[RValue]");
    d.rhs->accept(*this);
}

void PrinterVisitor::accept(BinaryOperation& b)
{
    print(std::string{type_name(b)});
    auto level_guard = LevelGuard{level};
    print("kind=" + BinaryOperation::kind_strings.at(b.kind));
    print("[LHS]");
    b.lhs->accept(*this);
    print("[RHS]");
    b.rhs->accept(*this);
}

void PrinterVisitor::accept(UnaryOperation& u)
{
    print(std::string{type_name(u)});
    auto level_guard = LevelGuard{level};
    print(std::string{"kind="} + u.kind_strings.at(u.kind));
    print("[Operand]");
    u.expr->accept(*this);
}

void PrinterVisitor::accept(IfElseExpr& i)
{
    print(std::string{type_name(i)});
    auto level_guard = LevelGuard{level};
    print("[Condition]");
    i.condition->accept(*this);
    print("[If_Body]");
    i.if_body->accept(*this);
    if (i.else_body) {
        print("[Else_Body]");
        i.else_body->accept(*this);
    }
}

void PrinterVisitor::accept(WhileExpr& w)
{
    print(std::string{type_name(w)});
    auto level_guard = LevelGuard{level};
    print("[Condition]");
    w.condition->accept(*this);
    print("[Body]");
    w.body->accept(*this);
}

void PrinterVisitor::accept(FunctionCall& f)
{
    print(std::string{type_name(f)});
    auto level_guard = LevelGuard{level};
    print(std::string{"name="} + f.callee_name);
    print("[Arguments]");
    for (const auto& arg : f.args) {
        arg->accept(*this);
    }
}

void PrinterVisitor::accept(Variable& v)
{
    print(std::string{type_name(v)});
    auto level_guard = LevelGuard{level};
    print("name=" + v.name);
}

void PrinterVisitor::accept(ArrayAccess& a)
{
    print(std::string{type_name(a)});
    auto level_guard = LevelGuard{level};
    print("name=" + a.name);
    print("[IndexExpressions]");
    for (const auto& expr : a.index_exprs) {
        expr->accept(*this);
    }
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

void PrinterVisitor::accept(ArrayExpr& arr)
{
    print(std::string{type_name(arr)});
    auto level_guard = LevelGuard{level};
    print("[Elements]");
    for (const auto& elem : arr.elements) {
        Expression* elem_raw = elem.get();
        elem_raw->accept(*this);
    }
}

void PrinterVisitor::accept(NullValue& v)
{
    print(std::string{type_name(v)});
}

void PrinterVisitor::accept(BlankExpr& b)
{
    print(std::string{type_name(b)});
}
