# Scheme_Capstone
This repository contains all files involved in the creation of a Scheme self-interpreter for my Capstone project.

## Repository contents:

1) /code/: Contains the interpreter implemented in direct style (interpreter.ml), the auxiliary functions used in the interpreter as well as the environment definition (interpreter_essentials.ml), the unparser for error raising (unparser.ml), the definition of Primitive functions or the default functions that exist in the environment when the interpreter is loaded (primitives.ml) as well as the test functions for my interpreter, interpreter auxiliary functions and primitive functions.

2) /code_cps/: Contains the files inside /code/ which are written in Continuation Passing Style. Work in progress, where I am in the midst of implementing the Call with Current Continuation construct, the Apply construct, and the tests for the interpreter.

3) /utils/: Contains the lexer, parser and instructions to add support for quotations in the interpreter for Scheme written in OCaml.

4) /scheme_code/: Contains the Scheme self-interpreter, tests for the self-interpreter and a file-parser for .scm files.

## Progress till now (24 March 2021):

1) Implemented an intepreter for Scheme in OCaml written in Direct Style, and completed testing.
2) Implemented an intepreter for Scheme in OCaml written in Continuation Passing Style, implemented call-cc and apply, and completed testing.
3) Added support for quotations in both the interpreter written in Direct Style and the interpreter written in Continuation Passing Style, and completed testing.
4) Added a lexer and parser (courtesy of my supervisor) to be used with the interpreter for Scheme written in OCaml, and completed testing of interpreters using actual Scheme code vs their AST representations
5) Implemented a self-interpreter for Scheme, and utilized Curry's YCombinator (variadic version implemented by Mayer Goldberg) for letrec expressions. Completed testing.
6) Defined a function that runs on Scheme _programs_, and defined a file-parser that parses .scm files into Scheme programs.
7) Finished implementation of _tower_ of self-interpreters, and tested up till a tower containing 3 self-interpreters running on top of each other on top of Scheme. 
8) To begin on adding a version of a self-interpreter where the primitives perform type-checking.

## Instructions to run the interpreters

### For the tower of self-interpreters
In Scheme, just load the file containing the function that runs a tower of self interpreter with user-indicated size _n_ and run any example quoted Scheme program
as input to the tower:
```
> (load "file_path/tower_self_interpreters.scm")
;;; Running a tower of self-interpreters of size 3 on (+ 1 2)
> (run-star-prog 3 '(+ 1 2))
3
;;; Running using the time function in Scheme

;;; Checking time taken to run a tower of self-interpreters of size 4 on factorial 5
> (time (run-star-prog-qq 4 '(letrec ((fac (lambda (n) (if (= n 0) 1 (* n (fac (- n 1))))))) (fac 5))))
(time (run-star-prog-qq 4 ...))
    24231 collections
    1051.968750000s elapsed cpu time, including 3.046875000s collecting
    1056.520110100s elapsed real time, including 3.300919100s collecting
    203944346576 bytes allocated, including 203944406912 bytes reclaimed
120
```

### For the self-interpreter
In Scheme, just load the file containing the self interpreter and run any example quoted Scheme program
as input to the interpreter:
```
> (load "file_path/self_interpreter.scm")
> (interpret '(+ 1 2))
3
```
To run the tests for the self-interpreter:
```
> (load "file_path/test_self_interpreter.scm")
;;; Running the following test function should return NO output, meaning that all tests have been passed.
> (test '())
```

### For the Scheme interpreters written in OCaml 
In the terminal where the directory is set to the Scheme_Capstone repository's path, run the command `dune utop`. `dune utop` automatically runs all relevant tests and will raise an error if any of the tests are violated. Then, run the following commands to run the interpreters on Scheme code (input as strings):

For the interpreter written in Direct Style:
```
# open Code;;
# open Interpreter;;
# interpreter_on_scheme_input "(+ 5 5)";;
- : Ast.exp_val = Code.Ast.Int 10
```
For the interpreter written in Continuation Passing Style:
```
# open Code_cps;;
# open Interpreter_cps;;
# interpreter_cps_on_scheme_input "(+ 5 5)";;
- : Ast_cps.exp_val = Code_cps.Ast_cps.Int 10
```
