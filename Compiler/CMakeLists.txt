cmake_minimum_required(VERSION 3.21)
project(Compiler)

set(CMAKE_CXX_STANDARD 20)

set(LexerSources
        Lexer.h
        Lexer.cpp
        )

set(ParserSources
        Parser.h
        Parser.cpp
        )

set(AnalysatorSources
        SemanticAnalysator.h
        SemanticAnalysator.cpp
        )

add_executable(Compiler main.cpp ${LexerSources} ${ParserSources} ${AnalysatorSources})
