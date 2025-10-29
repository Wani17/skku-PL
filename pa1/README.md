# PA1: Symbol Table Builder

## Overview
Construct per-scope symbol tables (global + one per function) by walking the parse tree produced from `B.g4`.

## Grade
10/10

## Objective
- Parse B programs using the provided grammar and construct symbol tables for:
  - Global scope (`$_global_$`)
  - Each function scope(`functionName`)
  - Each block scope (`functionName_$ScopeNum`)
- Each symbol stores: name and type (`auto`)
- Detect errors in B language and print them

## Tasks
- Use ANTLR4 to generate C++ lexer/parser/visitor from `B.g4`.
- Complete STEP 1 (`SymbolTableVisitor`) to construct symbole table.
- Visit declarations, parameters, and function definitions.
- Insert variable/function symbols into the correct scope.
- After traversal, print all symbol tables.
