#include "Lexer.h"
#include "Parser.h"

int main() {
    Lexer lexer(R"(tests\loops_if_test)");
    std::vector<Token> tokens = lexer.getAllTokens();
    Parser parser(tokens);
    ASTNode* root = parser.parseProgram();
    parser.writeTree(root);
}
