
#ifndef COMPILER_SEMANTICANALYSATOR_H
#define COMPILER_SEMANTICANALYSATOR_H

#include "Parser.h"

class SemanticAnalysator {
public:
    explicit SemanticAnalysator(ASTNode *rootNode) : root(rootNode) {}

    void analyzeTree();

private:
    ASTNode *root;
};


#endif //COMPILER_SEMANTICANALYSATOR_H
