open Ast
open Interpreter
open Interpreter_essentials

(* Testing for error: If the correct error is raised, test returns unit meaning that there is no exception raised. *)
   
exception Not_implemented_yet

(* Testing the eval function *)
let test_eval_integer candidate =
  let b0 = (candidate (Integer 1) empty_alist = Int 1)
  and b1 = (candidate (Integer 2) empty_alist = Int 2)
  and b2 = (candidate (Integer 100) empty_alist = Int 100)
  and b3 = (candidate (Integer (-1)) empty_alist = Int (-1))
  and b4 = (candidate (Integer (-2)) empty_alist = Int (-2))
  and b5 = (candidate (Integer (-100)) empty_alist = Int (-100))
  and b6 = (candidate (Integer 0) empty_alist = Int 0)
  in b0 && b1 && b2 && b3 && b4 && b5 && b6;;

let test_eval_integer_non_empty_env candidate =
  let b0 = (candidate (Integer 1) [("x", Int 5)]  = Int 1)
  and b1 = (candidate (Integer 2) [("y", Int 3)] = Int 2)
  and b2 = (candidate (Integer 100) [("x", Boolean true);
                                     ("y", String "env")] = Int 100)
  and b3 = (candidate (Integer (-1)) [("x", Boolean true);
                                      ("y", String "env")] = Int (-1))
  and b4 = (candidate (Integer (-2)) [("x", Boolean true);
                                      ("y", String "env")]  = Int (-2))
  and b5 = (candidate (Integer (-100)) [("x", Boolean true);
                                        ("y", String "env")] = Int (-100))
  and b6 = (candidate (Integer 0) [("x", Boolean true);
                                   ("y", String "env")] = Int 0)
  in b0 && b1 && b2 && b3 && b4 && b5 && b6;;

let test_eval_boolean candidate =
  let b0 = (candidate (Bool true) empty_alist = Boolean true)
  and b1 = (candidate (Bool false) empty_alist = Boolean false)
  in b0 && b1;;

let test_eval_boolean_non_empty_env candidate =
  let b0 = (candidate (Bool true) [("x", Boolean true);
                                   ("y", String "env")] = Boolean true)
  and b1 = (candidate (Bool false) [("x", Boolean true);
                                    ("y", String "env")] = Boolean false)
  in b0 && b1;;

let test_eval_string candidate =
  let b0 = (candidate (Str "hello") empty_alist = String "hello")
  and b1 = (candidate (Str "world") empty_alist = String "world")
  and b2 = (candidate (Str "self-interpreter") empty_alist = String "self-interpreter")
  and b3 = (candidate (Str "Hawk 9598") empty_alist = String "Hawk 9598")
  and b4 = (candidate (Str "Scheme 2020") empty_alist = String "Scheme 2020")
  in b0 && b1 && b2 && b3 && b4;;

let test_eval_string_non_empty_env candidate =
  let b0 = (candidate (Str "hello") [("x", Boolean true);
                                     ("y", String "env")] = String "hello")
  and b1 = (candidate (Str "world") [("x", Boolean true);
                                     ("y", String "env")] = String "world")
  and b2 = (candidate (Str "self-interpreter") [("x", Boolean true);
                                                ("y", String "env")] = String "self-interpreter")
  and b3 = (candidate (Str "Hawk 9598") [("x", Boolean true);
                                         ("y", String "env")] = String "Hawk 9598")
  and b4 = (candidate (Str "Scheme 2020") [("x", Boolean true);
                                           ("y", String "env")] = String "Scheme 2020")
  in b0 && b1 && b2 && b3 && b4;;

let test_eval_char candidate =
  let b0 = (candidate (Char 'c') empty_alist = Character 'c')
  and b1 = (candidate (Char 'd') empty_alist = Character 'd')
  and b2 = (candidate (Char 'f') empty_alist = Character 'f')
  and b3 = (candidate (Char '1') empty_alist = Character '1')
  and b4 = (candidate (Char '9') empty_alist = Character '9')
  in b0 && b1 && b2 && b3 && b4;;

let test_eval_char_non_empty_env candidate =
  let b0 = (candidate (Char 'c') [("x", Boolean true);
                                  ("y", String "env")]  = Character 'c')
  and b1 = (candidate (Char 'd') [("x", Boolean true);
                                  ("y", String "env")]  = Character 'd')
  and b2 = (candidate (Char 'f') [("x", Boolean true);
                                  ("y", String "env")]  = Character 'f')
  and b3 = (candidate (Char '1') [("x", Boolean true);
                                  ("y", String "env")]  = Character '1')
  and b4 = (candidate (Char '9') [("x", Boolean true);
                                  ("y", String "env")]  = Character '9')
  in b0 && b1 && b2 && b3 && b4;;

let test_eval_var candidate =
  let b0 = (candidate (Var "x") [("x", Int 6)] = Int 6)
  and b1 = (candidate (Var "x")[("x", Character 'a')] = Character 'a')
  and b2 = (candidate (Var "hello")[("x", Boolean true);
                                    ("hello", String "world")] = String "world")
  and b3 = (candidate (Var "y") [("x", Int 6);
                                 ("z", String "string");
                                 ("y", Boolean true)] = Boolean true)
  and b4 = (candidate (Var "z")[("a",  Pair (Int 6,
                                             Boolean true));
                                ("z", Pair
                                        (Pair (Int 5,
                                               Boolean true),
                                         Character '5'))] = Pair
                                                              (Pair
                                                                 (Int 5,
                                                                  Boolean true),
                                                               Character '5'))
  and b5 = (candidate (Var "v")[("a", String "variable");
                                ("b", Character 'c');
                                ("c", Int 5);
                                ("d", Boolean false);
                                ("v", Pair(Int 5,
                                           String "env"));
                                ("z", Int 0)] = Pair(Int 5,
                                                     String "env"))
  and b6 = (candidate (Var "f")[("f", String "first");
                                ("f", String "repeated")] = String "first")
  in b0 && b1 && b2 && b3 && b4 && b5 && b6;;

let test_eval_var_error candidate =
  let b0 = (try ignore (candidate (Var "x") empty_alist);
                failwith "bad lookup" with (Interpreter_essentials.Lookup_not_found "x") -> ())
  and b1 = (try ignore (candidate (Var "x") [("y", Int 0);
                                             ("a", String "false")]);
                failwith "bad lookup" with (Interpreter_essentials.Lookup_not_found "x") -> ())
  and b2 = (try ignore (candidate (Var "var") [("y", Int 0);
                                               ("a", String "false");
                                               ("variable", Int 1)]);
                failwith "bad lookup" with (Interpreter_essentials.Lookup_not_found "var") -> ())
  in b0; b1; b2;;


let test_eval_if candidate =
  (* If predicate is Boolean true *)
  let b0 = (candidate (If (Bool true,
                           Str "yes",
                           Str "no")) empty_alist = String "yes")
  and b1 = (candidate (If (Bool true,
                           Integer 1,
                           Integer 0)) empty_alist = Int 1)
  and b2 = (candidate (If (Bool true,
                           Char 'y',
                           Char 'n')) empty_alist = Character 'y')
  and b3 = (candidate (If (Bool true,
                           Var "y",
                           Var "z")) [("x", Int 0);
                                      ("y", Int 1);
                                      ("z", Int 2)] = Int 1)
  (* If predicate is Boolean false *)
  and b4 = (candidate (If (Bool false,
                           Str "yes",
                           Str "no")) empty_alist = String "no")
  and b5 = (candidate (If (Bool false,
                           Integer 1,
                           Integer 0)) empty_alist = Int 0)
  and b6 = (candidate (If (Bool false,
                           Char 'y',
                           Char 'n')) empty_alist = Character 'n')
  and b7 = (candidate (If (Var "x",
                           Var "y",
                           Var "z")) [("x", Boolean false);
                                      ("y", String "yes");
                                      ("z", String "no")] = String "no")
  (* If predicate is something other than Boolean false, evaluated to true. *)
  and b8 = (candidate (If (Integer 0,
                           Str "yes",
                           Str "no")) empty_alist = String "yes")
  and b9 = (candidate (If (Char 'a',
                           Integer 1,
                           Integer 0)) empty_alist = Int 1)
  and b10 = (candidate (If (Str "wayne",
                            Char 'y',
                            Char 'n')) empty_alist = Character 'y')
  and b11 = (candidate (If (Var "x",
                            Var "y",
                            Var "z")) [("x", Boolean true);
                                       ("y", String "yes");
                                       ("z", String "no")] = String "yes")
  in b0 && b1 && b2 && b3 && b4 && b5 && b6 && b7 && b8 && b9 && b10 && b11;;

let test_eval_if_with_primitives candidate =
  let b0 = (candidate (If (Apply (Var "=",
                                  [Integer 5; Integer 0]),
                           Bool true,
                           Bool false)) default_empty_alist = Boolean true)
  and b1 = (candidate (If (Apply (Var "<",
                                  [Integer 5; Integer 6; Integer 7; Integer 8]),
                           Integer 0,
                           Integer 1)) default_empty_alist = Int 0)
  and b2 = (candidate (If (Apply (Var "string?",
                                  [Str "5"; Str "a"; Str "hi"]),
                           Str "yes",
                           Str "no")) default_empty_alist = String "yes")
  in b0 && b1 && b2;;

let test_eval_apply candidate =
  let b0 = ((candidate (Apply
                          ((Lambda_abstraction
                              (Lambda
                                 (Args_list ["x"; "y"; "z"],
                                  If (Var "x",
                                      Var "y",
                                      Var "z")))),
                           [Bool true; Integer 1; Integer 2]))
               empty_alist) = Int 1)
  and b1 = ((candidate (Apply
                          ((Lambda_abstraction
                              (Lambda
                                 (Args_list ["x"; "y"; "z"],
                                  If (Var "x",
                                      Var "y",
                                      Var "z")))),
                           [Bool false; Integer 1; Integer 2]))
               empty_alist) = Int 2)
  and b2 = ((candidate (Apply
                          ((Lambda_abstraction
                              (Lambda
                                 (Args_list ["x"],
                                  Var "x"))),
                           [Char 'Y']))
               empty_alist) = Character 'Y')
  and b3 = ((candidate (Apply
                          ((Lambda_abstraction
                              (Lambda
                                 (Args_list ["x"; "y"],
                                  Var "x"))),
                           [Bool true; Bool false]))
               empty_alist) = Boolean true)
  and b4 = ((candidate (Apply
                          ((Lambda_abstraction
                              (Lambda
                                 (Args_list ["x"; "y"],
                                  Var "y"))),
                           [Bool true; Bool false]))
               empty_alist) = Boolean false)
  in b0 && b1 && b2 && b3 && b4;;

let test_eval_apply_error candidate =
  let b0 = (try ignore (candidate (Apply
                              ((If (Integer 1,
                                    Char 'Y',
                                    Char 'N')),
                               [Bool false; Integer 1; Char 'c']))
                          empty_alist);
            failwith "Error not occurring" with Interpreter.Error ("Not a procedure: 'Y'") -> ())
  and b1 = (try ignore (candidate (Apply
                                     ((If (Bool false,
                                           Char 'Y',
                                           Char 'N')),
                                      [Bool false; Integer 1; Char 'c']))
                          empty_alist);
                failwith "Error not occurring" with Interpreter.Error ("Not a procedure: 'N'") -> ())
  and b2 = (try ignore (candidate (Apply
                                     (Str "hello world",
                                      [Bool false; Integer 1; Char 'c']))
                          empty_alist);
                failwith "Error not occurring" with Interpreter.Error ("Not a procedure: \"hello world\"") -> ())
  and b3 = (try ignore (candidate (Apply
                                     (Char 'c',
                                      [Bool false; Integer 1; Char 'c']))
                          empty_alist);
                failwith "Error not occurring" with Interpreter.Error ("Not a procedure: 'c'") -> ())
  and b4 = (try ignore (candidate (Apply
                                     (Bool true,
                                      [Bool false; Integer 1; Char 'c']))
                          empty_alist);
                failwith "Error not occurring" with Interpreter.Error ("Not a procedure: true") -> ())
  and b5 = (try ignore (candidate (Apply
                                     (Var "test",
                                      [Bool false; Integer 1; Char 'c']))
                          [("test", Int 10)]);
                failwith "Error not occurring" with Interpreter.Error ("Not a procedure: 10") -> ())
  and b6 = (try ignore (candidate (Apply
                                     (Integer 5,
                                      [Bool false; Integer 1; Char 'c']))
                          empty_alist);
                failwith "Error not occurring" with Interpreter.Error ("Not a procedure: 5") -> ())
  in b0; b1; b2; b3; b4; b5; b6;;

(* Define the recursive functions we will be using to test let_rec_unary *)

let factorial =
  (Lambda_abstraction
     (Lambda (Args_list ["x"],
              Let_rec_unary
                (("factorial",
                  Lambda (Args_list ["x"],
                          If (Apply (Var "=", [Var "x"; Integer 0]),
                              Integer 1,
                              Apply (Var "*",
                                     [Var "x" ;
                                      Apply (Var "factorial",
                                             [Apply (Var "-",
                                                     [Var "x";
                                                      Integer 1])])])))),
                 Apply (Var "factorial",
                        [Var "x"])))));;

let factorial_star =
  (Lambda_abstraction
     (Lambda (Args_list ["x"],
              Let_rec
                ([("factorial",
                  Lambda (Args_list ["x"],
                          If (Apply (Var "=", [Var "x"; Integer 0]),
                              Integer 1,
                              Apply (Var "*",
                                     [Var "x" ;
                                      Apply (Var_rec ("factorial", 0),
                                             [Apply (Var "-",
                                                     [Var "x";
                                                      Integer 1])])]))))],
                 Apply (Var_rec ("factorial", 0),
                        [Var "x"])))));;

let addition =
  (Lambda_abstraction
     (Lambda (Args_list ["n1"; "n2"],
              Let_rec_unary
                (("addition",
                  Lambda(Args_list ["n1"; "n2"],
                         If (Apply (Var "=", [Var "n1";
                                              Integer 0]),
                             Var "n2",
                             Apply (Var "+",
                                    [Integer 1;
                                     Apply (Var "addition",
                                            [Apply (Var "-",
                                                    [Var "n1"; Integer 1]);
                                             Var "n2"])])))),
                 Apply (Var "addition",
                        [Var "n1"; Var "n2"])))));;
                                                     
(* Testing let_rec_unary *)                         
  
let test_eval_let_rec_unary candidate =
  (* Base case of the factorial function *)
  let b0 = (candidate (Apply (factorial,
                              [Integer 0])) default_empty_alist = Int 1)
  (* Non-base case of the factorial function *)
  and b1 = (candidate (Apply (factorial,
                              [Integer 1])) default_empty_alist = Int 1)
  and b2 = (candidate (Apply (factorial,
                              [Integer 5])) default_empty_alist = Int 120)
  and b3 = (candidate (Apply (factorial,
                              [Integer 9])) default_empty_alist = Int 362880)
  and b4 = (candidate (Apply (factorial,
                              [Integer 10])) default_empty_alist = Int 3628800)
  (* Base case of the addition function, defined recursively *)
  and b5 = (candidate (Apply (addition,
                              [Integer 0;
                               Integer 1000])) default_empty_alist = Int 1000)
  and b6 = (candidate (Apply (addition,
                              [Integer 0;
                               Integer (-5)])) default_empty_alist = Int (-5))
  (* Non-base case of the addition function *)
  and b7 = (candidate (Apply (addition,
                              [Integer 10;
                               Integer 25])) default_empty_alist = Int 35)
  and b8 = (candidate (Apply (addition,
                              [Integer 1000;
                               Integer 525])) default_empty_alist = Int 1525)
  and b9 = (candidate (Apply (addition,
                              [Integer 25;
                               Integer (-25)])) default_empty_alist = Int 0)
 in b0 && b1 && b2 && b3 && b4 && b5 && b6 && b7 && b8 && b9;;

assert (test_eval_integer eval);;
assert (test_eval_integer_non_empty_env eval);;
assert (test_eval_boolean eval);;
assert (test_eval_boolean_non_empty_env eval);;
assert (test_eval_string eval);;
assert (test_eval_string_non_empty_env eval);;
assert (test_eval_char eval);;
assert (test_eval_char_non_empty_env eval);;
assert (test_eval_var eval);;
(test_eval_var_error eval);; 
assert (test_eval_if eval);;
assert (test_eval_apply eval);;
(test_eval_apply_error eval);;
assert (test_eval_let_rec_unary eval);;
