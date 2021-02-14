# Scheme_Capstone

### This repository contains all files involved in the creation of a Scheme self-interpreter for my Capstone project.

### Repository contents:

1) /code/*: Contains the interpreter implemented in direct style (interpreter.ml), the auxiliary functions used in the interpreter as well as the environment definition (interpreter_essentials.ml), the unparser for error raising (unparser.ml), the definition of Primitive functions or the default functions that exist in the environment when the interpreter is loaded (primitives.ml) as well as the test functions for my interpreter, interpreter auxiliary functions and primitive functions.

2) /code_cps/*: Contains the files inside /code/* which are written in Continuation Passing Style. Work in progress, where I am in the midst of implementing the Call with Current Continuation construct, the Apply construct, and the tests for the interpreter.

### Progress till now:

1) Implemented an intepreter for Scheme in OCaml written in Direct Style, which however lacks the Scheme constructs of call-cc and apply.
2) Implemented an intepreter for Scheme in OCaml written in Continuation Passing Style, and am working on implementing call-cc and apply.
3) Working on the test cases for the variadic, fixed arity and mixed lambda abstractions, as well as the let construct for the DS interpreter.
4) Will implement the test cases for the interpreter written in CPS, which really is the same as the original modulo the provision of an initial continuation (fun v -> v)
