# Scheme Capstone Code Brief Documentation

## Table of Contents
1) Files in /code/* for OCaml interpreter for Scheme written in _direct style_
2) Files in /code_cps/* for OCaml interpreter for Scheme written in _continuation passing style_
3) Files in /scheme_code/* for Scheme self-interpreter and tower of self-interpreters

## OCaml interpreter for Scheme written in direct style

#### interpreter.ml
Contains implementation of the interpreter and other auxiliary functions for the interpreter
written in _direct style_.

#### interpreter_essentials.ml 
Contains implementation of the initial environment that contains the primitives,
the lookup and extend environment functions, and auxiliary functions that
convert between OCaml and Scheme lists.

#### primitives.ml
Contains the implementations of _all_ primitive procedures initialized inside our 
initial environment for the OCaml interpreter, with the implementations done
to mimic the behavior of the primitives in Scheme.

#### ast.ml
Contains the definitions of the expressible values for our OCaml interpreter
written in _direct style_.

#### unparser.ml
Contains auxiliary functions that help with pretty-printing error messages.

#### test_interpreter.ml
Contains the unit tests that are used to test for our interpreter written 
in _direct style_. Unit tests are expressions in AST Form.

#### test_interpreter_essentials.ml
Contains the unit tests that are used to test for our environment related functions
as well as the auxiliary functions that convert between OCaml and Scheme lists.

#### test_interpreter_on_scheme_exp.ml
Contains the unit tests that are used to test for our interpreter in direct style.
Unit tests are actual expressions written in actual Scheme syntax. 

#### test_primitives.ml
Contains the unit tests that are used to test each and every one of the primitive 
procedures defined in primitives.ml

## OCaml interpreter for Scheme written in CPS

#### interpreter_cps.ml
Contains implementation of the interpreter and other auxiliary functions for the interpreter
written in _continuation passing style_.
#### interpreter_essentials_cps.ml 
Contains implementation of the initial environment that contains the primitives,
the lookup and extend environment functions, and auxiliary functions that
convert between OCaml and Scheme lists.

#### primitives_cps.ml
Contains the implementations of _all_ primitive procedures initialized inside our 
initial environment for the OCaml interpreter, with the implementations done
to mimic the behavior of the primitives in Scheme

#### ast_cps.ml
Contains the definitions of the expressible values for our OCaml interpreter
written in _continuation passing style_. Note that there are differences compared
to the version written in _direct style_ due to our use of continuations.
#### unparser_cps.ml
Contains auxiliary functions that help with pretty-printing error messages

#### test_interpreter_cps.ml
Contains the unit tests that are used to test for our interpreter written in 
_continuation passing style_. Unit tests are expressions in AST Form.

#### test_interpreter_essentials_cps.ml
Contains the unit tests that are used to test for our environment related functions
as well as the auxiliary functions that convert between OCaml and Scheme lists.

#### test_interpreter_cps_on_scheme_exp.ml
Contains the unit tests that are used to test for our interpreter in direct style.
Unit tests are actual expressions written in actual Scheme syntax. 

#### test_primitives_cps.ml
Contains the unit tests that are used to test each and every one of the primitive 
procedures defined in primitives.ml

## Scheme self interpreter and tower of self_interpreters

#### self_interpreter.scm
Contains the interpreter, the environment and its related functions, the run
function and other auxiliary functions utilized in run.

#### test_self_interpreter.scm
Contains all the tests for the functions defined in self_interpreter.scm

#### file_parser.scm
Contains the definition of a file parser used to parse .scm files into Scheme
programs

#### test_file_parser.scm
The test file for our file parser.

#### executing_self_interpreter.scm
Contains the exact same content as self_interpreter.scm, but used as the
base self-interpreter in our tower of self-interpreters.

#### tower_self_interpreters.scm
Contains the definition of run-prog, run-2-prog, run-3-prog, run-star-prog and
other auxiliary functions that help visualize the tower of self-interpreters. Note
that run-prog refers to a tower of size 1, run-2-prog refers to a tower of size 2,
run-3-prog refers to a tower of size 3 and run-star-prog refers to a tower of
any size inputted by the user.

#### test_tower_self_interpreters.scm
Contains the unit tests to test our tower of size 1, 2, 3 and our tower
defined using run-star-prog for any n. Note that running tests for run-star-prog
using a value of n > 3 results in a very long runtime.

#### self_interpreter_typecheck.scm
Contains a type-checked version of self_interpreter.scm, where the arity and
type of the input arguments to our defined primitive functions are checked.

#### test_self_interpreter_typecheck.scm
Contains unit tests for self_interpreter_typecheck.scm

#### executing_self_interpreter_typecheck.scm
Contains a type-checked version of executing_self_interpreter.scm

#### tower_self_interpreters_typecheck.scm
Contains a type-checked version of tower_self_interpreters.scm, where all
levels of the tower are type-checked self-interpreters

#### test_tower_self_interpreters_typecheck.scm
Contains unit tests for tower_self_interpreters_typecheck.scm

