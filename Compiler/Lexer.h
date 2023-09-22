#ifndef COMPILER_LEXER_H
#define COMPILER_LEXER_H

#include <utility>
#include <vector>
#include <string>
#include <iostream>
#include <fstream>

enum TokenType{
    IDENTIFIER,
    KEYWORD,
    INTEGER,
    OPERATOR,
    SPECIAL_CHARACTER,
    COMMENT,
    END_OF_FILE
};

struct Token{
    TokenType type;
    std::string lexeme;
};

class Lexer {
public:
    Token getNextToken();

    std::vector<Token> getAllTokens();

    explicit Lexer(const std::string&  fajl) :position(0) {
        // Open file on the given path, read everything from it and then close the file

        std::ifstream inpfile(fajl);

        if (!inpfile) {
            std::cerr << "Error: Could not open the file." << std::endl;
            std::exit(1);
        }

        std::string line;
        while (std::getline(inpfile, line)) {
            code += line + '\n';
        }

        inpfile.close();
    }

private:
    Token scanIdentifier();
    Token scanInteger();
    Token scanOperator();
    Token scanSpecialCharacter();
    Token scanComment();

    std::string code;
    size_t position;

};


#endif //COMPILER_LEXER_H
