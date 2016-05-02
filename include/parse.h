#ifndef FUNLANG_PARSE_H
#define FUNLANG_PARSE_H

#include <boost/circular_buffer.hpp>

#include "ast_nodes.h"
#include "lex.h"

namespace Funlang
{

struct ParseError : std::exception
{
    ParseError(Token at, size_t lineno);

    const Token at;
    const size_t lineno;
};

struct UnexpectedTokenError : ParseError
{
    UnexpectedTokenError(Token::Kind expected, Token at, size_t lineno);

    const Token::Kind expected;
};

struct LValueExpected : ParseError
{
    LValueExpected(Token at, size_t lineno);
};

class Parser
{
public:
    Parser(Lexer& lexer);

    std::unique_ptr<AST::Program> parse_all();

private:    
    Lexer& lexer;
    boost::circular_buffer<std::unique_ptr<Token>> lookahead_buffer;
    Token* current_token;
    std::unique_ptr<Token> eof_token = nullptr;

    Token* lookahead(size_t offset);

    std::unique_ptr<Token> consume(char kind);
    std::unique_ptr<Token> consume(Token::Kind kind);

    auto program()
        -> std::unique_ptr<AST::Program>;

    auto functions()
        -> std::vector<std::unique_ptr<AST::Function>>;

        auto function()
            -> std::unique_ptr<AST::Function>;

            auto optparams()
                -> std::vector<std::unique_ptr<AST::Parameter>>;

                auto parameter(bool is_optional)
                    -> std::unique_ptr<AST::Parameter>;

            auto block()
                -> std::unique_ptr<AST::Block>;

            auto static_type()
                -> std::unique_ptr<AST::StaticTypeId>;

                auto optexprs()
                    -> std::vector<std::unique_ptr<AST::Expression>>;

                    auto expression()
                        -> std::unique_ptr<AST::Expression>;

                        auto assignment()
                            -> std::unique_ptr<AST::Expression>;

                            auto or_op()
                                -> std::unique_ptr<AST::Expression>;

                                auto and_op()
                                    -> std::unique_ptr<AST::Expression>;

                                    auto eq_neq()
                                        -> std::unique_ptr<AST::Expression>;

                                        auto conditional()
                                            -> std::unique_ptr<AST::Expression>;

                                            auto addsub()
                                                -> std::unique_ptr<AST::Expression>;

                                                auto muldiv()
                                                    -> std::unique_ptr<AST::Expression>;

                                                    auto primary()
                                                        -> std::unique_ptr<AST::Expression>;

                                                        auto definition()
                                                            -> std::unique_ptr<AST::Definition>;

                                                        auto if_else_expr()
                                                            -> std::unique_ptr<AST::Expression>;

                                                            auto else_tail()
                                                                -> std::unique_ptr<AST::Expression>;

                                                        auto while_expr()
                                                            -> std::unique_ptr<AST::WhileExpr>;

                                                        auto function_call()
                                                            -> std::unique_ptr<AST::FunctionCall>;

                                                            auto optargs()
                                                                -> std::vector<std::unique_ptr<AST::Expression>>;

                                                        auto array()
                                                            -> std::unique_ptr<AST::ArrayExpr>;

                                                        auto literal()
                                                            -> std::unique_ptr<AST::Literal>;

                                                        auto lvalue()
                                                            -> std::unique_ptr<AST::LValue>;

    template<typename NodeT, typename... Args>
    std::unique_ptr<NodeT> make_node(Args&&... args)
    {
        static_assert(std::is_base_of<AST::Node, NodeT>::value,
                      "NodeT must be derived from AST::Node");
        auto node = std::make_unique<NodeT>(std::forward<Args>(args)...);
        node->lineno = lexer.line();
        return node;
    }
};

}
#endif //FUNLANG_PARSE_H
