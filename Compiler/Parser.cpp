#include "Parser.h"

ASTNode *Parser::parseProgram() {
    auto *prog = new ASTNode{PROGRAM, "Program"};
    ASTNode *declarationList = parseDeclarationList();
    prog->children.push_back(declarationList);
    return prog;
}

ASTNode *Parser::parseDeclarationList() {
    auto *declList = new ASTNode{DECLARATION_LIST, "DeclarationList"};

    while (true) {
        ASTNode *decl = parseDeclaration();
        if (!decl) break;
        declList->children.push_back(decl);
    }

    return declList;
}

ASTNode *Parser::parseDeclaration() {
    while (tokens[current].type == COMMENT) current++;
    int cur = this->current;
    ASTNode *varDeclaration = parseVariableDeclaration();
    if (varDeclaration) return varDeclaration;

    this->current = cur;
    ASTNode *funDeclaration = parseFunctionDeclaration();
    if (funDeclaration) return funDeclaration;

    this->current = cur;
    ASTNode *structDeclaration = parseStructDeclaration();
    if (structDeclaration) return structDeclaration;

    if (tokens[current].type != END_OF_FILE) {
        std::cerr << "Parsing error: Expected a valid declaration.";
        exit(1);
    }
    return nullptr;
}

ASTNode *Parser::parseVariableDeclaration() {
    auto *varDecl = new ASTNode{VARIABLE_DECLARATION, "VariableDeclaration"};

    ASTNode *typeSpec = parseTypeSpecifier();
    if (typeSpec) varDecl->children.push_back(typeSpec);
    else return nullptr;

    if (current < tokens.size() && tokens[current].type == IDENTIFIER) {
        varDecl->children.push_back(new ASTNode{AST_IDENTIFIER, tokens[current].lexeme});
        current++;
    } else return nullptr;

    if (current < tokens.size() && tokens[current].lexeme == "[") {
        varDecl->children.push_back(new ASTNode{ARRAY_FLAG, "ArrayFlag"});
        current++;
        ASTNode *constExpr = parseConstantExpression();
        if (constExpr) varDecl->children.push_back(constExpr);
        else return nullptr;
        if (current < tokens.size() && tokens[current].lexeme == "]") {
            current++;
        } else return nullptr;
    }
    if (current < tokens.size() && tokens[current].lexeme == "=") {
        current++;
        ASTNode *condExpr = parseConditionalExpression();
        if (condExpr) varDecl->children.push_back(condExpr);
        else return nullptr;
    }
    if (current < tokens.size() && tokens[current].lexeme == ";") current++;
    else return nullptr;

    return varDecl;
}

ASTNode *Parser::parseTypeSpecifier() {
    if (current < tokens.size() && tokens[current].type == KEYWORD) {
        if (tokens[current].lexeme == "int") {
            current++;
            return new ASTNode{TYPE_SPECIFIER, "int"};
        } else if (tokens[current].lexeme == "char") {
            current++;
            return new ASTNode{TYPE_SPECIFIER, "char"};
        } else if (tokens[current].lexeme == "long") {
            current++;
            return new ASTNode{TYPE_SPECIFIER, "long"};
        } else if (tokens[current].lexeme == "void") {
            current++;
            return new ASTNode{TYPE_SPECIFIER, "void"};
        } else if (tokens[current].lexeme == "short") {
            current++;
            return new ASTNode{TYPE_SPECIFIER, "short"};
        } else if (tokens[current].lexeme == "float") {
            current++;
            return new ASTNode{TYPE_SPECIFIER, "float"};
        } else if (tokens[current].lexeme == "struct") {
            current++;
            if (current < tokens.size() && tokens[current].type == IDENTIFIER) {
                auto *str = new ASTNode{TYPE_SPECIFIER, "struct"};
                auto *ident = new ASTNode{AST_IDENTIFIER, tokens[current].lexeme};
                current++;
                str->children.push_back(ident);
                return str;
            } else return nullptr;
        } else return nullptr;
    }
    return nullptr;
}

ASTNode *Parser::parseFunctionDeclaration() {
    auto *funDecl = new ASTNode{FUNCTION_DECLARATION, "FunctionDeclaration"};
    ASTNode *typeSpec = parseTypeSpecifier();
    if (typeSpec) funDecl->children.push_back(typeSpec);
    else return nullptr;
    if (current < tokens.size() && tokens[current].type == IDENTIFIER) {
        funDecl->children.push_back(new ASTNode{AST_IDENTIFIER, tokens[current].lexeme});
        current++;
    } else return nullptr;

    if (current < tokens.size() && tokens[current].lexeme == "(") current++;
    else return nullptr;

    ASTNode *parList = parseParameterList();
    if (parList) funDecl->children.push_back(parList);
    else if (tokens[current].lexeme != ")") return nullptr;

    if (current < tokens.size() && tokens[current].lexeme == ")") current++;
    else return nullptr;

    ASTNode *comStat = parseCompoundStatement();
    if (comStat) funDecl->children.push_back(comStat);
    else return nullptr;

    return funDecl;
}

ASTNode *Parser::parseParameterList() {
    auto *paramList = new ASTNode{PARAMETER_LIST, "ParameterList"};

    ASTNode *par1 = parseParameter();
    if (par1) paramList->children.push_back(par1);
    else return nullptr;

    while (current < tokens.size() && tokens[current].lexeme == ",") {
        current++;
        ASTNode *par2 = parseParameter();
        if (par2) paramList->children.push_back(par2);
        else return nullptr;
    }

    return paramList;
}

ASTNode *Parser::parseParameter() {
    auto *param = new ASTNode{PARAMETER, "Parameter"};

    ASTNode *typeSpec = parseTypeSpecifier();
    if (typeSpec) param->children.push_back(typeSpec);
    else return nullptr;

    if (current < tokens.size() && tokens[current].type == IDENTIFIER) {
        param->children.push_back(new ASTNode{AST_IDENTIFIER, tokens[current].lexeme});
        current++;
    } else return nullptr;

    if (current < tokens.size() && tokens[current].lexeme == "[") {
        current++;
        if (current < tokens.size() && tokens[current].lexeme == "]") {
            param->children.push_back(new ASTNode{ARRAY_FLAG, "ArrayFlag"});
        } else return nullptr;
    }
    return param;
}

ASTNode *Parser::parseStructDeclaration() {
    auto *strDecl = new ASTNode{STRUCT_DECLARATION, "StructDeclaration"};

    if (current < tokens.size() && tokens[current].type == KEYWORD && tokens[current].lexeme == "struct") current++;
    else return nullptr;

    if (current < tokens.size() && tokens[current].type == IDENTIFIER) {
        strDecl->children.push_back(new ASTNode{AST_IDENTIFIER, tokens[current].lexeme});
        current++;
    } else return nullptr;

    if (current < tokens.size() && tokens[current].lexeme == "{") current++;
    else return nullptr;

    ASTNode *memList = parseMemberList();
    if (memList) strDecl->children.push_back(memList);
    else if (tokens[current].lexeme != "}") return nullptr;

    if (current < tokens.size() && tokens[current].lexeme == "}") current++;
    else return nullptr;

    if (current < tokens.size() && tokens[current].lexeme == ";") current++;
    else {
        std::cerr << "Parsing error: Expected ; at the end of struct declaration.";
        exit(1);
    }

    return strDecl;
}

ASTNode *Parser::parseMemberList() {
    auto *memList = new ASTNode{MEMBER_LIST, "MemberList"};

    while (true) {
        ASTNode *mem = parseMember();
        if (!mem) break;
        memList->children.push_back(mem);
    }

    return memList;
}

ASTNode *Parser::parseMember() {
    auto *member = new ASTNode{MEMBER, "Member"};

    ASTNode *typeSpec = parseTypeSpecifier();
    if (typeSpec) member->children.push_back(typeSpec);
    else return nullptr;

    if (current < tokens.size() && tokens[current].type == IDENTIFIER) {
        member->children.push_back(new ASTNode{AST_IDENTIFIER, tokens[current].lexeme});
        current++;
    } else return nullptr;

    if (current < tokens.size() && tokens[current].lexeme == "[") {
        current++;
        ASTNode *constExpr = parseConstantExpression();
        if (constExpr) member->children.push_back(constExpr);
        else return nullptr;
        current++;
        if (current < tokens.size() && tokens[current].lexeme == "]") {
            member->children.push_back(new ASTNode{ARRAY_FLAG, "ArrayFlag"});
            current++;
        } else return nullptr;
    }
    if (current < tokens.size() && tokens[current].lexeme == ";") current++;
    else {
        std::cerr << "Parsing Error: Expected ; at the end of member declaration.";
        exit(1);
    };

    return member;
}

ASTNode *Parser::parseCompoundStatement() {
    auto *compStat = new ASTNode{COMPOUND_STATEMENT, "CompoundStatement"};

    if (current < tokens.size() && tokens[current].lexeme == "{") current++;
    else return nullptr;

    ASTNode *statList = parseStatementList();
    if (!statList) return nullptr;

    if (current < tokens.size() && tokens[current].lexeme == "}") current++;
    else return nullptr;

    return statList;
}

ASTNode *Parser::parseStatementList() {
    while (tokens[current].type == COMMENT) current++;
    auto *statList = new ASTNode{STATEMENT_LIST, "StatementList"};

    while (true) {
        ASTNode *stat = parseStatement();
        if (!stat) break;
        statList->children.push_back(stat);
    }
    return statList;
}

ASTNode *Parser::parseStatement() {
    int cur = this->current;
    ASTNode *exprStatement = parseExpressionStatement();
    if (exprStatement) return exprStatement;

    this->current = cur;
    ASTNode *ifStat = parseIfStatement();
    if (ifStat) return ifStat;

    this->current = cur;
    ASTNode *whileStat = parseWhileStatement();
    if (whileStat) return whileStat;

    this->current = cur;
    ASTNode *forStat = parseForStatement();
    if (forStat) return forStat;

    this->current = cur;
    ASTNode *retStat = parseReturnStatement();
    if (retStat) return retStat;

    this->current = cur;
    ASTNode *varDecl = parseVariableDeclaration();
    if (varDecl) return varDecl;

    // this is for ternary operator that i dont have in my language, but i might add it
    this->current = cur;
    ASTNode *condStat = parseConditionalExpression();
    if (condStat) return condStat;

    return nullptr;
}

ASTNode *Parser::parseExpressionStatement() {

    ASTNode *expr = parseExpression();
    if (!expr) return nullptr;

    if (current < tokens.size() && tokens[current].lexeme == ";") current++;
    else return nullptr;

    return expr;
}

ASTNode *Parser::parseIfStatement() {
    auto *ifStat = new ASTNode{IF_STATEMENT, "IfStatement"};

    if (current < tokens.size() && tokens[current].type == KEYWORD && tokens[current].lexeme == "if") current++;
    else return nullptr;

    if (current < tokens.size() && tokens[current].lexeme == "(") current++;
    else {
        std::cerr << "Parsing Error: Expected ( in if statement.";
        exit(1);
    }

    ASTNode *expr = parseExpression();
    if (expr) ifStat->children.push_back(expr);
    else return nullptr;

    if (current < tokens.size() && tokens[current].lexeme == ")") current++;
    else {
        std::cerr << "Parsing Error: Expected ) in if statement.";
        exit(1);
    }

    ASTNode *compStat = parseCompoundStatement();
    if (compStat) ifStat->children.push_back(compStat);
    else {
        std::cerr << "Parsing Error: Expected compound statement in if statement.";
        exit(1);
    }

    if (current < tokens.size() && tokens[current].type == KEYWORD && tokens[current].lexeme == "else") {
        current++;
        ASTNode *compoStat = parseCompoundStatement();
        if (compoStat) ifStat->children.push_back(compoStat);
        else {
            std::cerr << "Parsing Error: Expected compound statement in if/else statement.";
            exit(1);
        }
    }

    return ifStat;
}

ASTNode *Parser::parseWhileStatement() {
    auto *whileStat = new ASTNode{WHILE_STATEMENT, "WhileStatement"};

    if (current < tokens.size() && tokens[current].type == KEYWORD && tokens[current].lexeme == "while")current++;
    else return nullptr;

    if (current < tokens.size() && tokens[current].lexeme == "(") current++;
    else {
        std::cerr << "Parsing Error: Expected ( in while statement.";
        exit(1);
    }

    ASTNode *expr = parseExpression();
    if (expr) whileStat->children.push_back(expr);
    else {
        std::cerr << "Parsing Error: Expected expression in while statement.";
        exit(1);
    }

    if (current < tokens.size() && tokens[current].lexeme == ")") current++;
    else {
        std::cerr << "Parsing Error: Expected ) in while statement.";
        exit(1);
    }

    ASTNode *compStat = parseCompoundStatement();
    if (compStat) whileStat->children.push_back(compStat);
    else {
        std::cerr << "Parsing Error: Expected compound statement in while statement.";
        exit(1);
    }
    return whileStat;
}

ASTNode *Parser::parseForStatement() {
    auto *forStat = new ASTNode{FOR_STATEMENT, "ForStatement"};

    if (current < tokens.size() && tokens[current].type == KEYWORD && tokens[current].lexeme == "for") current++;
    else return nullptr;

    if (current < tokens.size() && tokens[current].lexeme == "(")current++;
    else {
        std::cerr << "Parsing Error: Expected ( in for statement.";
        exit(1);
    }

    ASTNode *forInit = parseForInitializer();
    if (forInit) forStat->children.push_back(forInit);
    else {
        std::cerr << "Parsing Error: Expected for initializer in for statement.";
        exit(1);
    }

    ASTNode *expr = parseExpressionStatement();
    if (expr) forStat->children.push_back(expr);
    else {
        std::cerr << "Parsing Error: Expected expression statement in for statement.";
        exit(1);
    }

    ASTNode *forUpd = parseForUpdater();
    if (forUpd) forStat->children.push_back(forUpd);
    else {
        std::cerr << "Parsing Error: Expected for updater in for statement.";
        exit(1);
    }

    if (current < tokens.size() && tokens[current].lexeme == ")") current++;
    else {
        std::cerr << "Parsing Error: Expected ) in for statement";
        exit(1);
    }

    ASTNode *compStat = parseCompoundStatement();
    if (compStat) forStat->children.push_back(compStat);
    else {
        std::cerr << "Parsing Error: Expected compound statement in for statement.";
        exit(1);
    }

    return forStat;
}

ASTNode *Parser::parseForInitializer() {
    ASTNode *expr = parseExpression();
    if (expr) return expr;

    ASTNode *varDecl = parseVariableDeclaration();
    if (varDecl) return varDecl;

    return nullptr;
}

ASTNode *Parser::parseForUpdater() {
    ASTNode *expr = parseExpression();
    if (expr) return expr;

    ASTNode *varDecl = parseVariableDeclaration();
    if (varDecl) return varDecl;

    return nullptr;
}

ASTNode *Parser::parseReturnStatement() {
    auto *retStat = new ASTNode{RETURN_STATEMENT, "ReturnStatement"};

    if (current < tokens.size() && tokens[current].type == KEYWORD && tokens[current].lexeme == "return") current++;
    else return nullptr;

    ASTNode *expr = parseExpression();
    if (expr) retStat->children.push_back(expr);
    else {
        std::cerr << "Parsing Error: Expected expression in return statement";
        exit(1);
    }

    if (current < tokens.size() && tokens[current].lexeme == ";") current++;
    else {
        std::cerr <<"Parsing Error: Expected ; in return statement";
        exit(1);
    }

    return retStat;
}

ASTNode *Parser::parseExpression() {
    return parseAssignmentExpression();
}

ASTNode *Parser::parseAssignmentExpression() {
    ASTNode *contExpr = parseConditionalExpression();

    if (current < tokens.size() && tokens[current].lexeme == "=") {
        current++;
        ASTNode *assExpr = parseAssignmentExpression();

        auto *assExpr2 = new ASTNode{ASSIGNMENT_EXPRESSION, "AssignmentExpression"};
        assExpr2->children.push_back(contExpr);
        assExpr2->children.push_back(assExpr);
        return assExpr2;
    }
    return contExpr;
}

ASTNode *Parser::parseConditionalExpression() {
    return parseLogicalOrExpression();
    // ovde bi trebao da se doda ternarni operator u budućnosti
}

ASTNode *Parser::parseLogicalOrExpression() {
    ASTNode *left = parseLogicalAndExpression();

    if (current < tokens.size() && tokens[current].lexeme == "||") {
        current++;
        ASTNode *right = parseLogicalOrExpression();
        if (right) {
            auto *logOrExpr = new ASTNode{LOGICAL_OR_EXPRESSION, "LogicalOrExpression"};
            logOrExpr->children.push_back(left);
            logOrExpr->children.push_back(new ASTNode{AST_OPERTOR, "||"});
            logOrExpr->children.push_back(right);
            return logOrExpr;
        } else return nullptr;
    }

    return left;
}

ASTNode *Parser::parseLogicalAndExpression() {
    ASTNode *left = parseEqualityExpression();

    if (current < tokens.size() && tokens[current].lexeme == "&&") {
        current++;
        ASTNode *right = parseLogicalAndExpression();
        if (right) {
            auto *logAndExpr = new ASTNode{LOGICAL_AND_EXPRESSION, "LogicalAndExpression"};
            logAndExpr->children.push_back(left);
            logAndExpr->children.push_back(new ASTNode{AST_OPERTOR, "&&"});
            logAndExpr->children.push_back(right);

            return logAndExpr;
        } else return nullptr;
    }

    return left;
}

ASTNode *Parser::parseEqualityExpression() {
    ASTNode *left = parseRelationalExpression();
    if (current < tokens.size() && (tokens[current].lexeme == "==" || tokens[current].lexeme == "!=")) {
        std::string lexeme = tokens[current].lexeme;
        current++;
        ASTNode *right = parseEqualityExpression();
        if (right) {
            auto *eqExpr = new ASTNode{EQUALITY_EXPRESSION, "EqualityExpression"};
            eqExpr->children.push_back(left);
            eqExpr->children.push_back(new ASTNode{AST_OPERTOR, lexeme});
            eqExpr->children.push_back(right);
            return eqExpr;
        } else return nullptr;
    }
    return left;
}

ASTNode *Parser::parseRelationalExpression() {
    ASTNode *left = parseAdditiveExpression();

    if (current < tokens.size() &&
        (tokens[current].lexeme == "<=" || tokens[current].lexeme == ">=" || tokens[current].lexeme == "<" ||
         tokens[current].lexeme == ">")) {
        std::string lexeme = tokens[current].lexeme;
        current++;
        ASTNode *right = parseRelationalExpression();
        if (right) {
            auto *relExpr = new ASTNode{RELATIONAL_EXPRESSION, "RelationalExpression"};
            relExpr->children.push_back(left);
            relExpr->children.push_back(new ASTNode{AST_OPERTOR, lexeme});
            relExpr->children.push_back(right);
            return relExpr;
        } else return nullptr;
    }

    return left;
}

ASTNode *Parser::parseAdditiveExpression() {
    ASTNode *left = parseMultiplicativeExpression();

    if (current < tokens.size() && (tokens[current].lexeme == "-" || tokens[current].lexeme == "+")) {
        std::string lexeme = tokens[current].lexeme;
        current++;
        ASTNode *right = parseAdditiveExpression();
        if (right) {
            auto *addExpr = new ASTNode{ADDITIVE_EXPRESSION, "AdditiveExpression"};
            addExpr->children.push_back(left);
            addExpr->children.push_back(new ASTNode{AST_OPERTOR, lexeme});
            addExpr->children.push_back(right);
            return addExpr;
        }
    }

    return left;
}

ASTNode *Parser::parseMultiplicativeExpression() {
    ASTNode *left = parseUnaryExpression();

    if (current < tokens.size() &&
        (tokens[current].lexeme == "%" || tokens[current].lexeme == "/" || tokens[current].lexeme == "*")) {
        std::string lexeme = tokens[current].lexeme;
        current++;
        ASTNode *right = parseMultiplicativeExpression();
        if (right) {
            auto *mulExpr = new ASTNode{MULTIPLICATIVE_EXPRESSION, "MultiplicativeExpression"};
            mulExpr->children.push_back(left);
            mulExpr->children.push_back(new ASTNode{AST_OPERTOR, lexeme});
            mulExpr->children.push_back(right);
            return mulExpr;
        } else return nullptr;
    }

    return left;
}

ASTNode *Parser::parseUnaryExpression() {
    if (current < tokens.size() && (tokens[current].lexeme == "!" || tokens[current].lexeme == "-")) {
        std::string lexeme = tokens[current].lexeme;
        current++;

        ASTNode *uExpr = parseUnaryExpression();
        if (uExpr) {
            auto *uExpr2 = new ASTNode{UNARY_EXPRESSION, "UnaryExpression"};
            uExpr2->children.push_back(new ASTNode{AST_OPERTOR, lexeme});
            uExpr2->children.push_back(uExpr);
            return uExpr2;
        } else return nullptr;
    } else return parsePostfixExpression();

}

ASTNode *Parser::parsePostfixExpression() {
    ASTNode *primaryExpr = parsePrimaryExpression();

    while (current < tokens.size()) {
        if (tokens[current].lexeme == "[") {
            current++;
            ASTNode *expr = parseExpression();
            if (current < tokens.size() && tokens[current].lexeme == "]") {
                current++;
                auto *arrInd = new ASTNode{ARRAY_FLAG, "ArrayIndexing"};
                arrInd->children.push_back(primaryExpr);
                arrInd->children.push_back(expr);
                primaryExpr = arrInd;
            } else return nullptr;
        } else if (tokens[current].lexeme == "(") {
            current++;
            ASTNode *argList = parseArgumentExpressionList();
            if (current < tokens.size() && tokens[current].lexeme == ")") {
                current++;
                auto *funCall = new ASTNode{FUNCTION_CALL, "FunctionCall"};
                funCall->children.push_back(primaryExpr);
                funCall->children.push_back(argList);
                primaryExpr = funCall;
            } else return nullptr;
        } else if (tokens[current].lexeme == ".") {
            current++;
            if (current < tokens.size() && tokens[current].type == IDENTIFIER) {
                auto *memAcc = new ASTNode{MEMBER_ACCESS, "MemberAccess"};
                memAcc->children.push_back(primaryExpr);
                memAcc->children.push_back(new ASTNode{AST_IDENTIFIER, tokens[current].lexeme});
                current++;
                primaryExpr = memAcc;
            } else return nullptr;
        } else if (tokens[current].lexeme == "++" || tokens[current].lexeme == "--") {
            if (current > 0 && tokens[current - 1].type == IDENTIFIER) {
                std::string lexeme = tokens[current].lexeme;
                current++;
                auto *incDec = new ASTNode{INC_DEC, "IncDec"};
                incDec->children.push_back(primaryExpr);
                incDec->children.push_back(new ASTNode{AST_OPERTOR, lexeme});
                primaryExpr = incDec;
            } else {
                return nullptr;
            }
        } else {
            break;
        }
    }

    return primaryExpr;
}


ASTNode *Parser::parsePrimaryExpression() {
    if (current < tokens.size()) {
        if (tokens[current].type == IDENTIFIER) {
            current++;
            return new ASTNode{AST_IDENTIFIER, tokens[current - 1].lexeme};
        } else if (tokens[current].type == INTEGER) {
            current++;
            return new ASTNode{AST_NUMBER, tokens[current - 1].lexeme};
        } else if (tokens[current].lexeme == "(") {
            current++;
            ASTNode *expr = parseExpression();
            if (current < tokens.size() && tokens[current].lexeme == ")") {
                current++;
                if (expr) return expr;
            } else return nullptr;
        }
    }

    return nullptr;
}

ASTNode *Parser::parseArgumentExpressionList() {
    auto *paramList = new ASTNode{ARGUMENT_LIST, "ArgumentList"};

    ASTNode *par1 = parseArgument();
    if (par1) paramList->children.push_back(par1);
    else return nullptr;

    while (current < tokens.size() && tokens[current].lexeme == ",") {
        current++;
        ASTNode *par2 = parseArgument();
        if (par2) paramList->children.push_back(par2);
        else return nullptr;
    }

    return paramList;
}

ASTNode *Parser::parseConstantExpression() {
    if (current < tokens.size() && tokens[current].type == INTEGER) {
        current++;
        return new ASTNode{AST_NUMBER, tokens[current - 1].lexeme};
    }
    return nullptr;
}

ASTNode *Parser::parseArgument() {
    auto *param = new ASTNode{ARGUMENT, "Argument"};
    ASTNode *arg = parseExpression();
    if (arg) {
        param->children.push_back(arg);
        return param;
    } else return nullptr;
}

void Parser::writeTree(ASTNode *node, int depth) {
    for (int i = 0; i < depth; ++i) {
        std::cout << "   ";
    }

    std::cout << node->lexeme << std::endl;

    for (ASTNode *child: node->children) {
        writeTree(child, depth + 1);
    }

}
