#include "token.h"

const std::string Funlang::Token::valid_chars = ",:;(){}!=+-*/<>";

std::ostream& Funlang::operator<<(std::ostream& os, const Token& t)
{
    return os << t.to_string();
}