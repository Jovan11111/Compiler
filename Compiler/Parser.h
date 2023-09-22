#ifndef COMPILER_PARSER_H
#define COMPILER_PARSER_H

#include <iostream>
#include <utility>
#include <vector>
#include "Lexer.h"

enum ASTNodeType {
    PROGRAM,
    DECLARATION_LIST,
    DECLARATION,
    VARIABLE_DECLARATION,
    TYPE_SPECIFIER,
    FUNCTION_DECLARATION,
    FUNCTION_CALL,
    PARAMETER_LIST,
    PARAMETER,
    STRUCT_DECLARATION,
    MEMBER_LIST,
    MEMBER,
    ARRAY_MEMBER,
    COMPOUND_STATEMENT,
    STATEMENT_LIST,
    STATEMENT,
    EXPRESSION_STATEMENT,
    IF_STATEMENT,
    WHILE_STATEMENT,
    FOR_STATEMENT,
    FOR_INITIALIZER,
    FOR_UPDATER,
    RETURN_STATEMENT,
    EXPRESSION,
    ASSIGNMENT_EXPRESSION,
    CONDITIONAL_EXPRESSION,
    LOGICAL_OR_EXPRESSION,
    LOGICAL_AND_EXPRESSION,
    EQUALITY_EXPRESSION,
    RELATIONAL_EXPRESSION,
    ADDITIVE_EXPRESSION,
    MULTIPLICATIVE_EXPRESSION,
    UNARY_EXPRESSION,
    POSTFIX_EXPRESSION,
    PRIMARY_EXPRESSION,
    AST_IDENTIFIER,
    AST_OPERTOR,
    CONSTANT_EXPRESSION,
    ARRAY_FLAG,
    MEMBER_ACCESS,
    INC_DEC,
    AST_NUMBER,
    ARGUMENT_LIST,
    ARGUMENT
};

struct ASTNode {
    ASTNodeType type;
    std::string lexeme;
    std::vector<ASTNode *> children;
};

class Parser {
public:
    explicit Parser(std::vector<Token> tokens) : tokens(std::move(tokens)), current(0) {}

    void writeTree(ASTNode *node, int depth = 0);

    ASTNode *parseProgram();

    ASTNode *parseDeclarationList();

    ASTNode *parseDeclaration();

    ASTNode *parseVariableDeclaration();

    ASTNode *parseTypeSpecifier();

    ASTNode *parseFunctionDeclaration();

    ASTNode *parseParameterList();

    ASTNode *parseParameter();

    ASTNode *parseStructDeclaration();

    ASTNode *parseMemberList();

    ASTNode *parseMember();

    ASTNode *parseCompoundStatement();

    ASTNode *parseStatementList();

    ASTNode *parseStatement();

    ASTNode *parseExpressionStatement();

    ASTNode *parseIfStatement();

    ASTNode *parseWhileStatement();

    ASTNode *parseForStatement();

    ASTNode *parseForInitializer();

    ASTNode *parseForUpdater();

    ASTNode *parseReturnStatement();

    ASTNode *parseExpression();

    ASTNode *parseAssignmentExpression();

    ASTNode *parseConditionalExpression();

    ASTNode *parseLogicalOrExpression();

    ASTNode *parseLogicalAndExpression();

    ASTNode *parseEqualityExpression();

    ASTNode *parseRelationalExpression();

    ASTNode *parseAdditiveExpression();

    ASTNode *parseMultiplicativeExpression();

    ASTNode *parseUnaryExpression();

    ASTNode *parsePostfixExpression();

    ASTNode *parsePrimaryExpression();

    ASTNode *parseArgumentExpressionList();

    ASTNode *parseConstantExpression();

    ASTNode *parseArgument();


private:
    std::vector<Token> tokens;
    size_t current;
};


#endif //COMPILER_PARSER_H
