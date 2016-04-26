#ifndef FUNLANG_LEX_H
#define FUNLANG_LEX_H

#include <iostream>
#include <memory>
#include <string>
#include <unordered_map>
#include <vector>

#include "token.h"

namespace Funlang
{

class Lexer
{
public:
    Lexer(std::istream& is)
        : is{is}
    {
    }

    std::unique_ptr<Token> get_token()
    {
        if (look_ahead() == '\0') return nullptr;

        if (auto token = get_number_or_sign()) {
            return token;
        }

        if (auto token = get_id_or_keyword()) {
            return token;
        }

        char ch = consume();
        if (ch == '=')
        {
            if (look_ahead() != '=')
            {
                return std::make_unique<Token>(ch);
            }
            consume();
            return std::make_unique<Token>(Token::EQ);
        }
        else if (ch == '!')
        {
            if (look_ahead() != '=')
            {
                return std::make_unique<Token>(ch);
            }
            consume();
            return std::make_unique<Token>(Token::NEQ);
        }
        else if (ch == '<' || ch == '>')
        {
            if (look_ahead() != '=')
            {
                return std::make_unique<Token>(ch);
            }
            consume();
            return ch == '<'
                   ? std::make_unique<Token>(Token::LE)
                   : std::make_unique<Token>(Token::GE);
        }
        else if (Token::valid_chars.find(ch) != std::string::npos)
        {
            return std::make_unique<Token>(ch);
        }

        return std::make_unique<ErrorToken>(
                "Unexpected '" + std::string{ch}  + "'",
                ch, lineno);
    }

    std::vector<std::unique_ptr<Token>> get_tokens()
    {
        std::vector<std::unique_ptr<Token>> tokens;
        std::unique_ptr<Token> token;
        while ((token = get_token())) {
            tokens.push_back(std::move(token));
        }
        return tokens;
    }

    char look_ahead()
    {
        strip_ws();
        if (is.eof()) return '\0';
        return is.peek();
    }

    size_t line() const { return lineno; }

private:
    std::istream& is;

    size_t lineno = 1;

    size_t line_offset = 0;

    bool ws_stripped = false;

    std::unordered_map<std::string, Token> keywords =
    {
            { "else", Token{Token::ELSE} },
            { "false", Token{Token::FALSE} },
            { "function", Token{Token::FUNCTION} },
            { "if", Token{Token::IF} },
            { "let", Token{Token::LET} },
            { "null", Token{Token::NULLVAL} },
            { "true", Token{Token::TRUE} },
            { "while", Token{Token::WHILE} }
    };

    std::unique_ptr<Token> get_number_or_sign()
    {
        std::string token_str;
        bool float_num = false;
        while (std::isdigit(look_ahead()) || look_ahead() == '.') {
            char peek = consume();
            if (peek == '.') {
                if (!float_num) {
                    float_num = true;
                }
                else {
                    return std::make_unique<ErrorToken>(
                            "Multiple dots in number " + token_str + peek,
                            peek, lineno);
                }
            }
            token_str += peek;
        }
        if (!token_str.empty()) {
            if (token_str == ".") {
                return std::make_unique<ErrorToken>(
                        "Number expected", '.', lineno);
            }
            if (!float_num) {
                int value = std::stoi(token_str);
                return std::make_unique<Int>(value);
            }
            else {
                float value = std::stof(token_str);
                return std::make_unique<Float>(value);
            }
        }
        return nullptr;
    }

    std::unique_ptr<Token> get_id_or_keyword()
    {
        std::string token_str;
        if (std::isalpha(look_ahead()) || look_ahead() == '_') {
            token_str += consume();
            while (std::isalnum(look_ahead()) || look_ahead() == '_') {
                // stop forming the name if whitespace was stripped
                if (ws_stripped) {
                    break;
                }
                token_str += consume();
            }
        }
        auto keyword = keywords.find(token_str);
        if (keyword != std::end(keywords))
        {
            return std::make_unique<Token>(keyword->second);
        }
        if (!token_str.empty())
        {
            return std::make_unique<Id>(token_str);
        }
        return nullptr;
    }

    void strip_ws()
    {
        if (ws_stripped) return;
        if (!std::isspace(is.peek())) {
            return;
        }

        do {
            char c = is.get();
            if (c == '\n') {
                ++line_offset;
            }
        } while (std::isspace(is.peek()));

        ws_stripped = true;
    }

    void update_position()
    {
        lineno += line_offset;
        line_offset = 0;
    }

    char consume()
    {
        char c = look_ahead();
        update_position();
        is.ignore();
        ws_stripped = false;
        return c;
    }
};

}

#endif //FUNLANG_LEX_H
