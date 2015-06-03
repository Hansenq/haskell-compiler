
# Imperative Compiler

Compiler for a basic imperative language, written in Haskell.

Trinity Term 2015


- `langDef.hs` contains the definition of the language.
- `langEval.hs` contains code that evaluates the language.
- `langTypes.hs` contains code that checks the types in the language.
- `fib.hk` and `test.hk` are files written in the language, used for testing.


TODO:
-- Differentiating between "int" and "bool" static declarations
-- Number of passes to go through program for soft typing.

- Memoisation
- Function Inlining
- Common Subexpression Simplification
    - Convert to Tree
    - Plug in 2nd program into leaves of tree
    - Convert into logic and find all in leaves
    - Consolidation, Dead Code Elimination
- Hoisting

- Inferenced Static Typing
- Static Typing with Annotations
- Soft Typing