#include "Lexer.h"

// list of all the operators that lexer can scan
std::vector<std::string> operators = {
        "++", "--", // unary operators
        "+", "-", "*", "/", "%", // arithmetic operators
        "<", "<=", ">", ">=", "==", "!=", // relational operators
        "&&", "||", "!", // logical operators
        "&", "|", "<<", ">>", "~", "^", // bitwise operators
        "=", "+=", "-=", "*=", "/=", "%=" // assignment operators
};

// list of all keywords that lexer can scan
std::vector<std::string> keywords = {
        "char", "int", "long", "float", "short",
        "struct", "break", "if", "else", "switch", "case", "enum",
        "extern", "return", "continue", "const", "static", "typedef",
        "for", "void", "while", "sizeof", "volatile"
};

// list of all special characters that lexer can scan
std::vector<char> special_characters = {
        '#', '(', ')', '{', '}', '[', ']', '.', ',', ';', ':', '"', '\'', '\\'
};

// reads character by character from code, decides which TokenType the token is, and return it
Token Lexer::getNextToken() {
    while (position < code.length()) {
        if (std::isspace(code[position])) {
            position++;
            continue;
        }
        if (code[position] == '/' and code[position + 1] == '/') return scanComment();

        for (std::string op: operators) {
            if (op[0] == code[position]) return scanOperator();
        }

        for (char c: special_characters) {
            if (c == code[position]) return scanSpecialCharacter();
        }

        if (std::isdigit(code[position])) return scanInteger();

        if (std::isalpha(code[position]) || code[position] == '_') { return scanIdentifier(); }

    }
    return {TokenType::END_OF_FILE, ""};
}

// Reads a name from code, and then decides if it is a keyword, or an identifier
Token Lexer::scanIdentifier() {
    std::string lexeme;
    while (position < code.length() && (std::isalnum(code[position]) or code[position] == '_')) {
        lexeme += code[position++];
    }
    for (const std::string &key: keywords) {
        if (key == lexeme) return {TokenType::KEYWORD, lexeme};
    }

    return {TokenType::IDENTIFIER, lexeme};
}

// Reads a number from code, and then decides if it is a float or integer
Token Lexer::scanInteger() {
    std::string lexeme;
    while (position < code.length() && std::isdigit(code[position])) {
        lexeme += code[position++];
    }
    return {TokenType::INTEGER, lexeme};
}

// Reads from code and decides which operator is used
Token Lexer::scanOperator() {
    std::string lexeme;
    lexeme += code[position++];
    if (position < code.length() and
        (code[position] == '+' or code[position] == '-' or code[position] == '=' or code[position] == '&' or
         code[position] == '|' or code[position] == '<' or code[position] == '>'))
        lexeme += code[position++];
    return {TokenType::OPERATOR, lexeme};
}

// Reads from code and decides which special character is used
Token Lexer::scanSpecialCharacter() {
    switch (code[position++]) {
        case '{' :
            return {TokenType::SPECIAL_CHARACTER, "{"};
        case '}' :
            return {TokenType::SPECIAL_CHARACTER, "}"};
        case '(' :
            return {TokenType::SPECIAL_CHARACTER, "("};
        case ')' :
            return {TokenType::SPECIAL_CHARACTER, ")"};
        case '[' :
            return {TokenType::SPECIAL_CHARACTER, "["};
        case ']' :
            return {TokenType::SPECIAL_CHARACTER, "]"};
        case '#':
            return {TokenType::SPECIAL_CHARACTER, "#"};
        case '.':
            return {TokenType::SPECIAL_CHARACTER, "."};
        case ';':
            return {TokenType::SPECIAL_CHARACTER, ";"};
        case ',':
            return {TokenType::SPECIAL_CHARACTER, ","};
        case ':':
            return {TokenType::SPECIAL_CHARACTER, ":"};
        case '"':
            return {TokenType::SPECIAL_CHARACTER, "\""};
        case '\'':
            return {TokenType::SPECIAL_CHARACTER, "\'"};
        case '\\':
            return {TokenType::SPECIAL_CHARACTER, "\\"};
        default:
            return {};
    }
}

// Reads from code and decides what is commented and how far
Token Lexer::scanComment() {
    std::string lexeme;
    while (position < code.length() and code[position] != '\n') lexeme += code[position++];
    return {TokenType::COMMENT, lexeme};
}

std::vector<Token> Lexer::getAllTokens() {
    std::vector<Token> ret;
    while (true) {
        Token token = getNextToken();
        ret.push_back(token);
        if (token.type == TokenType::END_OF_FILE) break;
    }
    return ret;
}

