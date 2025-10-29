# Programming Languages Project Assignments (2024-1)
**Instructor:** Prof. Hwansoo Han


Two compiler-building assignments using ANTLR to parse a tiny B language and translate it to C with strict type inference.

## PA1: Symbol Table Builder
- **Objective:** Parse B programs and build scoped symbol tables (global + per-function), then print them.
- **Grade:** 10/10
- **Key Features:**
  - ANTLR4 Visitor pattern
  - Build symbol tables for variables functions, and scopes
  - Keep all types as `auto` in PA1 (no type inference)
  - Detect errors in B language and print them
  - Print table in specified format

## PA2: B→C Translator with Type Inference
- **Objective:** Infer types for variables, parameters, and return values, then output valid C code without warnings.
- **Grade:** 18/20
- **Key Features:**
  - Strict typing rules (no mixed types)
  - Type inference from first assignment, return statements, and first callsite
  - Detect errors: inconsistent types, arity mismatch, missing return type, undefined types
  - Produce compilable C code
