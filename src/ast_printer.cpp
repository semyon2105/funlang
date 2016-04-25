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

void PrinterVisitor::accept(TypeId& t)
{
    print(std::string{type_name(t)});
    auto level_guard = LevelGuard{level};
    std::string kind;
    switch (t.kind) {
    case TypeId::Kind::Single:
        kind = "single";
        break;
    case TypeId::Kind::Array:
        kind = "array";
        break;
    }

    std::string type_name = t.name == "" ? "<Nothing>" : t.name;
    print(std::string{"name="} + type_name + ", kind=" + kind);
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
    print("[IndexExpression]");
    a.index_expr->accept(*this);
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

std::string to_string(const ArrayLiteral& array_lit)
{
    std::string str {"["};
    for (const auto& elem : array_lit.elements) {
        Literal* elem_raw = elem.get();
        if (auto boolean = dynamic_cast<BoolValue*>(elem_raw)) {
            str += boolean ? "True" : "False";
        }
        else if (auto integer = dynamic_cast<IntValue*>(elem_raw)) {
            str += std::to_string(integer->value);
        }
        else if (auto fp_num = dynamic_cast<FloatValue*>(elem_raw)) {
            str += std::to_string(fp_num->value);
        }
        else if (auto null_lit = dynamic_cast<NullValue*>(elem_raw)) {
            str += "null";
        }
        else if (auto arr_lit = dynamic_cast<ArrayLiteral*>(elem_raw)) {
            str += to_string(*arr_lit);
        }
        str += ", ";
    }
    str.resize(str.size() - 2);
    str += "]";
    return str;
}

void PrinterVisitor::accept(ArrayLiteral& arr)
{
    print(std::string{type_name(arr)});
    auto level_guard = LevelGuard{level};
    print("elements=" + to_string(arr));
}

void PrinterVisitor::accept(NullValue& v)
{
    print(std::string{type_name(v)});
}

void PrinterVisitor::accept(BlankExpr& b)
{
    print(std::string{type_name(b)});
}
