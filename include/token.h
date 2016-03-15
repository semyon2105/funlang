#ifndef FUNLANG_TOKEN_H
#define FUNLANG_TOKEN_H

#include <iostream>
#include <string>

namespace Funlang
{

class Token
{
public:
    enum Kind
    {
        BOOL = 256,
        ELSE,
        EQ,
        FALSE,
        FLOAT,
        FUNCTION,
        GE,
        ID,
        IF,
        INT,
        LE,
        LET,
        NEQ,
        NULLVAL,
        TRUE,
        WHILE,
        ERROR
    };

    static const std::string valid_chars;

    static std::string kind_to_string(Kind kind)
    {
        if (kind < 256)
        {
            char ch = static_cast<char>(kind);
            if (Token::valid_chars.find(ch) != std::string::npos)
                return std::string{"'" + std::string{ch} + "'"};
            else
                return std::string{"'" + std::string{ch} + "'"};
        }
        switch (kind)
        {
            case Token::BOOL:       return "BOOL";
            case Token::ELSE:       return "ELSE";
            case Token::EQ:         return "\'==\'";
            case Token::ERROR:      return "ERROR";
            case Token::FALSE:      return "FALSE";
            case Token::FLOAT:      return "FLOAT";
            case Token::FUNCTION:   return "FUNCTION";
            case Token::GE:         return "\'>=\'";
            case Token::ID:         return "ID";
            case Token::IF:         return "IF";
            case Token::INT:        return "INT";
            case Token::LE:         return "\'<=\'";
            case Token::LET:        return "LET";
            case Token::NEQ:        return "\'!=\'";
            case Token::NULLVAL:    return "NULL";
            case Token::TRUE:       return "TRUE";
            case Token::WHILE:      return "WHILE";
            default:
                return "UNKNOWN'"
                       + std::to_string(static_cast<size_t>(kind))
                       + "'";
        }
    }

    static std::string kind_to_string(char ch)
    {
        return kind_to_string(static_cast<Kind>(ch));
    }

    const Kind kind;

    Token(Kind kind)
        : kind{kind} {}

    Token(char ch)
        : kind{static_cast<Kind>(ch)} {}

    virtual std::string to_string() const
    {
        return "<" + kind_to_string(kind) + ">";
    }
};

class Id : public Token
{
public:
    const std::string name;

    Id(const std::string& name)
        : Token{ID}, name{name} {}

    std::string to_string() const override
    {
        return "<" + kind_to_string(kind) + ", " + name + ">";
    }
};

class Bool : public Token
{
public:
    const bool value;

    Bool(bool value)
        : Token{BOOL}, value{value} {}

    std::string to_string() const override
    {
        return "<" + kind_to_string(kind) + ", "
               + (value ? "true" : "false") + ">";
    }
};

class Int : public Token
{
public:
    const int value;

    Int(int value)
        : Token{INT}, value{value} {}
    
    std::string to_string() const override
    {
        return "<" + kind_to_string(kind) + ", "
               + std::to_string(value) + ">";
    }
};

class Float : public Token
{
public:
    const float value;

    Float(float value)
        : Token{FLOAT}, value{value} {}

    std::string to_string() const override
    {
        return "<" + kind_to_string(kind) + ", "
               + std::to_string(value) + ">";
    }
};



class Error : public Token
{
public:
    const std::string errmsg;
    const char badchar;
    const size_t lineno;
    const size_t colno;

    Error(const std::string& errmsg, char badchar, size_t lineno, size_t colno)
        : Token{ERROR},
          errmsg{errmsg},
          badchar{badchar},
          lineno{lineno},
          colno{colno}
    {
    }

    std::string to_string() const override
    {
        return "<" + kind_to_string(kind)
        + "{" + errmsg + "}, " + kind_to_string(badchar)
        + ", line: " + std::to_string(lineno)
        + ", col: " + std::to_string(colno) + ">";
    }
};

const std::string Token::valid_chars = ",:;(){}!=+-*/<>";

std::ostream& operator<<(std::ostream& os, const Token& t)
{
    return os << t.to_string();
}

}

#endif //FUNLANG_TOKEN_H