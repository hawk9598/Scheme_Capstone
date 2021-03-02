# Scheme_Capstone

### This repository contains all files involved in the creation of a Scheme self-interpreter for my Capstone project.

### Repository contents:

1) /code/: Contains the interpreter implemented in direct style (interpreter.ml), the auxiliary functions used in the interpreter as well as the environment definition (interpreter_essentials.ml), the unparser for error raising (unparser.ml), the definition of Primitive functions or the default functions that exist in the environment when the interpreter is loaded (primitives.ml) as well as the test functions for my interpreter, interpreter auxiliary functions and primitive functions.

2) /code_cps/: Contains the files inside /code/ which are written in Continuation Passing Style. Work in progress, where I am in the midst of implementing the Call with Current Continuation construct, the Apply construct, and the tests for the interpreter.

3) /utils/: Contains the lexer, parser and instructions to add support for quotations in the interpreter for Scheme written in OCaml.

4) /scheme_code/: Contains the Scheme self-interpreter file.

### Progress till now:

1) Implemented an intepreter for Scheme in OCaml written in Direct Style, and completed testing.
2) Implemented an intepreter for Scheme in OCaml written in Continuation Passing Style, implemented call-cc and apply, and completed testing.
3) Added support for quotations in both the interpreter written in Direct Style and the interpreter written in Continuation Passing Style, but will have to add testing for this.
4) Added a lexer and parser (courtesy of my supervisor) to be used with the interpreter for Scheme written in OCaml, and will add tests involving actual Scheme code for the two aforementioned interpreters.
5) Started on self-interpreter in Scheme, and implemented some of the auxiliary functions required.
