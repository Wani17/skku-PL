# PA2: B→C Translator with Type Inference
## Overview
Extend PA1 by inferring concrete types from `auto` and generating valid C code with no type warnings.

## Grade
18/20
## Objective
- Build a constraint-based type inference system.
- Deduce types of:
  - Variable initializers
  - Expressions (binary, unary, logical, relational)
  - Function parameters & returns
  - Ternary expressions
- Detect semantic errors (type mismatch, void misuse, undefined variables).
- Emit fully typed C code from the parse tree:
  - Replace `auto` with correct C types.
  - Generate function signatures.
  - Preserve program structure.

## Tasks
- Complete STEP 2 (`TypeAnalysisVisitor`) to infer/update symbol table types.
- Complete STEP 3 (`PrintTreeVisitor`) to print valid C with inferred types.
- Detect and report errors:
  - inconsistent variable types
  - inconsistent parameter types or function arity mismatch
  - invalid/missing return type
  - undefined types (never inferred)
- Output must compile cleanly:  
  `g++ -Wconversion -Wall -pedantic output.c`
