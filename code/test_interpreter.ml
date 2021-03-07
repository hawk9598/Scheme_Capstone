open Utils.Syntax
open Ast
open Interpreter
open Interpreter_essentials
open Unparser

exception Not_implemented_yet

(* Testing the eval function *)

(* Defining unit tests for the integer type expression *)
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

(* Defining unit tests for the boolean type expression *)
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

(* Defining unit tests for the string type expression *)
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

(* Defining unit tests for the character type expression *)
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

(* Defining unit tests for the variable type expression *)
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

(* Defining unit tests for the if type expression *)
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

(* Defining unit tests for the apply and different types of lambda abstractions *)
let test_eval_apply_fixed_arity candidate =
  let b0 = ((candidate (Apply
                          ((Lambda_abstraction
                              (Args_list ["x"; "y"; "z"],
                               If (Var "x",
                                   Var "y",
                                   Var "z"))),
                           [Bool true; Integer 1; Integer 2]))
               empty_alist) = Int 1)
  and b1 = ((candidate (Apply
                          ((Lambda_abstraction
                              (Args_list ["x"; "y"; "z"],
                               If (Var "x",
                                   Var "y",
                                   Var "z"))),
                           [Bool false; Integer 1; Integer 2]))
               empty_alist) = Int 2)
  and b2 = ((candidate (Apply
                          ((Lambda_abstraction
                              (Args_list ["x"],
                               Var "x")),
                           [Char 'Y']))
               empty_alist) = Character 'Y')
  and b3 = ((candidate (Apply
                          ((Lambda_abstraction
                              (Args_list ["x"; "y"],
                               Var "x")),
                           [Bool true; Bool false]))
               empty_alist) = Boolean true)
  and b4 = ((candidate (Apply
                          ((Lambda_abstraction
                              (Args_list ["x"; "y"],
                               Var "y")),
                           [Bool true; Bool false]))
               empty_alist) = Boolean false)
  in b0 && b1 && b2 && b3 && b4;;

(* Defining functions that operate on Scheme lists for testing variadic lambda abstractions *)
let sum_of_scheme_list =
  (fun vs ->
    begin
      match vs with
      |v1 :: [] ->    
        let rec visit v =
          begin match v with
          |Null -> 0 
          |Pair(x, y) ->
            begin match x with
            |Int i->
              i + visit y
            |_ -> raise (Interpreter_essentials.Error
                           (Printf.sprintf
                              "Error in sum_of_scheme_list: Not an int: %s"
                              (show_exp_val x)))
            end
          |_ -> raise (Interpreter_essentials.Error
                           (Printf.sprintf
                              "Error in sum_of_scheme_list: Not a Pair or List: %s"
                              (show_exp_val v)))
          end
        in Int (visit v1)
      |_ -> raise (Interpreter_essentials.Error
                     (Printf.sprintf
                        "Error in sum_of_scheme_list: Incorrect argument count in sum_of_scheme_list: %s"
                        (show_list show_exp_val vs)))
    end)

         
let test_eval_apply_variadic candidate =
  let b0 = ((candidate (Apply
                          ((Lambda_abstraction
                              (Args "x",
                               Apply (Var "sum_list",
                                      [Var "x"]))),
                           [Integer 1]))
               [("sum_list", Primitive sum_of_scheme_list)]) = Int 1)
  and b1 = ((candidate (Apply
                          ((Lambda_abstraction
                              (Args "x",
                               Apply (Var "sum_list",
                                      [Var "x"]))),
                           [Integer 5; Integer 10; Integer 1000; Integer (-200); Integer (-10)]))
               [("sum_list", Primitive sum_of_scheme_list)]) = Int 805)
  and b2 = ((candidate (Apply
                          ((Lambda_abstraction
                              (Args "x",
                               Apply (Var "car",
                                      [Var "x"]))),
                           [Str "car"; Integer 10; Char 'c']))
               default_empty_alist) = String "car")
  and b3 = ((candidate (Apply
                          ((Lambda_abstraction
                              (Args "x",
                               Apply (Var "cdr",
                                      [Var "x"]))),
                           [Integer 10; Str "cdr"; Bool true]))
               default_empty_alist) = Pair(String "cdr",
                                           Pair(Boolean true,
                                                Null)))
  and b4 = ((candidate (Apply
                          ((Lambda_abstraction
                              (Args "x",
                               Apply (Var "pair?",
                                      [Var "x"]))),
                           [Integer 10; Bool true; Char 'c']))
               default_empty_alist) = Boolean true)
  and b5 = ((candidate (Apply
                          ((Lambda_abstraction
                              (Args "x",
                               Apply (Var "car",
                                      [Apply (Var "cdr",
                                              [Var "x"])]))),
                           [Integer 10; Str "me"; Bool true; Char 'y']))
               default_empty_alist) = String "me")
         
         
  in b0 && b1 && b2 && b3 && b4 && b5;;

let test_eval_apply_improper candidate =
  let b0 = ((candidate (Apply
                          ((Lambda_abstraction
                              (Args_improper_list (["x"; "y"; "z"], "args"),
                               Apply (Var "+",
                                      [Var "x"; Var "y"; Var "z"; (Apply (Var "car",
                                                                          [Var "args"]))]))),
                           [Integer 10; Integer 100; Integer (-20); Integer (-10); Str "not_an_int"]))
               default_empty_alist) = Int 80)
  and b1 = ((candidate (Apply
                          ((Lambda_abstraction
                              (Args_improper_list (["x"; "y"; "z"], "args"),
                               Apply (Var "+",
                                      [Var "x"; Var "y"; Var "z"; (Apply (Var "car",
                                                                          [Var "args"]))]))),
                           [Integer 10; Integer 100; Integer (-20); Integer (-20); Integer 20; Str "not_an_int"]))
               default_empty_alist) = Int 70)
  and b2 = ((candidate (Apply
                          ((Lambda_abstraction
                              (Args_improper_list (["x"; "y"; "z"], "args"),
                               Apply (Var "+",
                                      [Var "x"; Var "y"; Var "z"]))),
                           [Integer 10; Integer 20; Integer 30]))
               default_empty_alist) = Int 60)
  and b3 = ((candidate (Apply
                          ((Lambda_abstraction
                              (Args_improper_list (["x"; "y"; "z"], "args"),
                               Apply (Var "+",
                                      [Var "x"; Var "y"; Var "z"; (Apply (Var "car",
                                                                          [Var "args"]));
                                       (Apply (Var "car",
                                               [Apply (Var "cdr",
                                                       [Var "args"])]))]))),
                           [Integer (-10); Integer (-20); Integer 100; Integer 10; Integer 20; Integer 1000]))
               default_empty_alist) = Int 100)
  in b0 && b1 && b2 && b3;;

                                  
                                  
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

(* Defining the unit tests for the let expression*)
let test_eval_let candidate =
  let b0 = ((candidate (Let ([("x", Integer 5);
                              ("y", Integer 7)],
                             Apply(Var "+",
                                   [Var "x"; Var "y"])))
               default_empty_alist) = Int 12)
  and b1 = ((candidate (Let ([("x", Bool true);
                              ("y", Bool false)],
                             If (Var "x",
                                 Str "true",
                                 Var "y")))
               default_empty_alist) = String "true")
                             
  (* Testing whether local definitions of functions work *)
  and b2 = ((candidate (Let ([("is_more_than_hundred?",
                               Lambda_abstraction
                                 (Args_list ["x"],
                                  If (Apply (Var ">",
                                             [Var "x"; Integer 100]),
                                      Bool true,
                                      Bool false)));
                              ("small_number", Integer 1);
                              ("big_number", Integer 1000)],
                             If (Apply (Var "=",
                                        [Integer 5; Integer 5; Integer 5]),
                                 Apply (Var "is_more_than_hundred?",
                                        [Var "small_number"]),
                                 Apply (Var "is_more_than_hundred?",
                                        [Var "big_number"]))))
               default_empty_alist) = Boolean false)
             
  in b0 && b1 && b2;;

(* Define the recursive functions we will be using to test let_rec *)
let factorial_star =
  (Lambda_abstraction
     (Args_list ["x"],
      Let_rec
        ([("factorial",
           (Args_list ["x"],
            If (Apply (Var "=", [Var "x"; Integer 0]),
                Integer 1,
                Apply (Var "*",
                       [Var "x" ;
                        Apply (Var_rec ("factorial", 0),
                               [Apply (Var "-",
                                       [Var "x";
                                        Integer 1])])]))))],
          Apply (Var_rec ("factorial", 0),
                 [Var "x"]))));;

let addition_star =
  (Lambda_abstraction
     (Args_list ["n1"; "n2"],
      Let_rec
        ([("addition",
           (Args_list ["n1"; "n2"],
            If (Apply (Var "=", [Var "n1";
                                 Integer 0]),
                Var "n2",
                Apply (Var "+",
                       [Integer 1;
                        Apply (Var_rec ("addition", 0),
                               [Apply (Var "-",
                                       [Var "n1"; Integer 1]);
                                Var "n2"])]))))],
          Apply (Var_rec ("addition", 0),
                 [Var "n1"; Var "n2"]))));;

let even_mutual_rec =
  (Lambda_abstraction
     (Args_list ["x"],
      Let_rec
        (["even",
          (Args_list ["x"],
           If(Apply (Var "=", [Var "x";
                               Integer 0]),
              Bool true,
              Apply (Var_rec ("odd", 1),
                     [Apply(Var "-",
                            [Var "x"; Integer 1])])));
           "odd",
           (Args_list ["x"],
            If(Apply (Var "=", [Var "x";
                                Integer 0]),
               Bool false,
               Apply (Var_rec ("even", 0),
                      [Apply(Var "-",
                             [Var "x"; Integer 1])])))],
          Apply (Var_rec ("even", 0),
                 [Var "x"]))));;

let odd_mutual_rec =
  (Lambda_abstraction
     (Args_list ["x"],
      Let_rec
        (["even",
          (Args_list ["x"],
           If(Apply (Var "=", [Var "x";
                               Integer 0]),
              Bool true,
              Apply (Var_rec ("odd", 1),
                     [Apply(Var "-",
                            [Var "x"; Integer 1])])));
           "odd",
           (Args_list ["x"],
            If(Apply (Var "=", [Var "x";
                                Integer 0]),
               Bool false,
               Apply (Var_rec ("even", 0),
                      [Apply(Var "-",
                             [Var "x"; Integer 1])])))],
           Apply (Var_rec ("odd", 1),
                  [Var "x"]))));;

let ternary_mutual_rec =
  (Lambda_abstraction
     (Args_list ["x"],
      Let_rec
        (["ternary",
          (Args_list ["x"],
           If(Apply (Var "=", [Var "x";
                               Integer 0]),
              Bool true,
              Apply (Var_rec ("preternary", 2),
                     [Apply(Var "-",
                            [Var "x"; Integer 1])])));
           "postternary",
           (Args_list ["x"],
            If(Apply (Var "=", [Var "x";
                                Integer 0]),
               Bool false,
               Apply (Var_rec ("ternary", 0),
                      [Apply(Var "-",
                             [Var "x"; Integer 1])])));
            "preternary",
            (Args_list ["x"],
             If(Apply (Var "=", [Var "x";
                                 Integer 0]),
                Bool false,
                Apply (Var_rec ("postternary", 1),
                       [Apply(Var "-",
                              [Var "x"; Integer 1])])))],
            Apply (Var_rec ("ternary", 0),
                   [Var "x"]))));;
      
let postternary_mutual_rec =
  (Lambda_abstraction
     (Args_list ["x"],
      Let_rec
        (["ternary",
          (Args_list ["x"],
           If(Apply (Var "=", [Var "x";
                               Integer 0]),
              Bool true,
              Apply (Var_rec ("preternary", 2),
                     [Apply(Var "-",
                            [Var "x"; Integer 1])])));
           "postternary",
           (Args_list ["x"],
            If(Apply (Var "=", [Var "x";
                                Integer 0]),
               Bool false,
               Apply (Var_rec ("ternary", 0),
                      [Apply(Var "-",
                             [Var "x"; Integer 1])])));
           "preternary",
           (Args_list ["x"],
            If(Apply (Var "=", [Var "x";
                                Integer 0]),
               Bool false,
               Apply (Var_rec ("postternary", 1),
                      [Apply(Var "-",
                             [Var "x"; Integer 1])])))],
          Apply (Var_rec ("postternary", 1),
                 [Var "x"]))));;

let preternary_mutual_rec =
  (Lambda_abstraction
     (Args_list ["x"],
      Let_rec
        (["ternary",
          (Args_list ["x"],
           If(Apply (Var "=", [Var "x";
                               Integer 0]),
              Bool true,
              Apply (Var_rec ("preternary", 2),
                     [Apply(Var "-",
                            [Var "x"; Integer 1])])));
              "postternary",
              (Args_list ["x"],
               If(Apply (Var "=", [Var "x";
                                   Integer 0]),
                  Bool false,
                  Apply (Var_rec ("ternary", 0),
                         [Apply(Var "-",
                                [Var "x"; Integer 1])])));
               "preternary",
               (Args_list ["x"],
                If(Apply (Var "=", [Var "x";
                                    Integer 0]),
                   Bool false,
                   Apply (Var_rec ("postternary", 1),
                          [Apply(Var "-",
                                 [Var "x"; Integer 1])])))],
               Apply (Var_rec ("preternary", 2),
                      [Var "x"]))));;

(* Defining a function that allows us to create repeated, randomized unit tests *)

 let repeat n_init thunk =
  assert (n_init >= 0);
  let rec loop n =
    if n = 0
    then true
    else thunk () && loop (pred n)
  in loop n_init;;

(* Defining unit test for the let_rec expression *)
let test_eval_let_rec candidate =
  (* Testing for single argument to let_rec *)
  
  (* Base case of the factorial function *)
  let b0 = (candidate (Apply (factorial_star,
                              [Integer 0])) default_empty_alist = Int 1)
  (* Non-base case of the factorial function *)
  and b1 = (candidate (Apply (factorial_star,
                              [Integer 1])) default_empty_alist = Int 1)
  and b2 = (candidate (Apply (factorial_star,
                              [Integer 5])) default_empty_alist = Int 120)
  and b3 = (candidate (Apply (factorial_star,
                              [Integer 9])) default_empty_alist = Int 362880)
  and b4 = (candidate (Apply (factorial_star,
                              [Integer 10])) default_empty_alist = Int 3628800)
  (* Base case of the addition function, defined recursively *)
  and b5 = (candidate (Apply (addition_star,
                              [Integer 0;
                               Integer 1000])) default_empty_alist = Int 1000)
  and b6 = (candidate (Apply (addition_star,
                              [Integer 0;
                               Integer (-5)])) default_empty_alist = Int (-5))
  (* Non-base case of the addition function *)
  and b7 = (candidate (Apply (addition_star,
                              [Integer 10;
                               Integer 25])) default_empty_alist = Int 35)
  and b8 = (candidate (Apply (addition_star,
                              [Integer 1000;
                               Integer 525])) default_empty_alist = Int 1525)
  and b9 = (candidate (Apply (addition_star,
                              [Integer 25;
                               Integer (-25)])) default_empty_alist = Int 0)
  (* Testing for mutual recursion to let_rec *)

  (* Base case of the even function *)
  and b10 = (candidate (Apply (even_mutual_rec,
                               [Integer 0])) default_empty_alist = Boolean true)
  (* Non-base case of the even function *)
  and b11 = (repeat 10
               (fun () ->
                 let n = Random.int 100 in
                 let result = (candidate (Apply (even_mutual_rec,
                                                 [Integer (2 * n)]))
                                 default_empty_alist = Boolean true)
                 in result))
  and b12 = (repeat 10
               (fun () ->
                 let n = Random.int 100 in
                 let result = (candidate (Apply (even_mutual_rec,
                                                 [Integer (2 * n + 1)]))
                                 default_empty_alist = Boolean false)
                 in result))
          
  (* Base case of the odd function *)
  and b13  = (candidate (Apply (odd_mutual_rec,
                                [Integer 0])) default_empty_alist = Boolean false)
  (* Non-base case of the odd function *)
  and b14 = (repeat 10
               (fun () ->
                 let n = Random.int 100 in
                 let result = (candidate (Apply (odd_mutual_rec,
                                                 [Integer (2 * n)]))
                                 default_empty_alist = Boolean false)
                 in result))
  and b15 = (repeat 10
               (fun () ->
                 let n = Random.int 100 in
                 let result = (candidate (Apply (odd_mutual_rec,
                                                 [Integer (2 * n + 1)]))
                                 default_empty_alist = Boolean true)
                 in result))
          
  (* Base case of the ternary function *)
  and b16 = (candidate (Apply (ternary_mutual_rec,
                               [Integer 0])) default_empty_alist = Boolean true)
  (* Base case of the postternary function *)
  and b17 = (candidate (Apply (postternary_mutual_rec,
                               [Integer 0])) default_empty_alist = Boolean false)
  (* Base case of the preternary function *)
  and b18 = (candidate (Apply (preternary_mutual_rec,
                               [Integer 0])) default_empty_alist = Boolean false)
  (* Non-base case of the 3 mutually recursive functions *)

  (* For the true cases *)
  and b19 = (repeat 10
               (fun () ->
                 let n = Random.int 100 in
                 let result = (candidate (Apply (ternary_mutual_rec,
                                                 [Integer (3 * n)]))
                                 default_empty_alist = Boolean true)
                 in result))
  and b20 = (repeat 10
               (fun () ->
                 let n = Random.int 100 in
                 let result = (candidate (Apply (postternary_mutual_rec,
                                                 [Integer (3 * n + 1)]))
                                 default_empty_alist = Boolean true)
                 in result))
  and b21 = (repeat 10
               (fun () ->
                 let n = Random.int 100 in
                 let result = (candidate (Apply (preternary_mutual_rec,
                                                 [Integer (3 * n + 2)]))
                                 default_empty_alist = Boolean true)
                 in result))
  (* For the remaining false cases *)
  and b22 = (repeat 10
               (fun () ->
                 let n = Random.int 100 in
                 let result = (candidate (Apply (ternary_mutual_rec,
                                                 [Integer (3 * n + 1)]))
                                 default_empty_alist = Boolean false)
                 in result))
  and b23 = (repeat 10
               (fun () ->
                 let n = Random.int 100 in
                 let result = (candidate (Apply (postternary_mutual_rec,
                                                 [Integer (3 * n + 2)]))
                                 default_empty_alist = Boolean false)
                 in result))
  and b24 = (repeat 10
               (fun () ->
                 let n = Random.int 100 in
                 let result = (candidate (Apply (preternary_mutual_rec,
                                                 [Integer (3 * n)]))
                                 default_empty_alist = Boolean false)
                 in result))
  and b25 = (repeat 10
               (fun () ->
                 let n = Random.int 100 in
                 let result = (candidate (Apply (ternary_mutual_rec,
                                                 [Integer (3 * n + 2)]))
                                 default_empty_alist = Boolean false)
                 in result))
  and b26 = (repeat 10
               (fun () ->
                 let n = Random.int 100 in
                 let result = (candidate (Apply (postternary_mutual_rec,
                                                 [Integer (3 * n)]))
                                 default_empty_alist = Boolean false)
                 in result))
  and b27 = (repeat 10
               (fun () ->
                 let n = Random.int 100 in
                 let result = (candidate (Apply (preternary_mutual_rec,
                                                 [Integer (3 * n + 1)]))
                                 default_empty_alist = Boolean false)
                 in result))
  in b0 && b1 && b2 && b3 && b4 && b5 && b6 && b7 && b8 && b9 && b10 && b11 && b12 && b13 && b14 && b15 && b16 && b17 && b18 && b19 && b20 && b21 && b22 && b23 && b24 && b25 && b26 && b27;;

(* Test for quotation of base values *)
let test_eval_quote_base candidate =
  (* Test for integers *)
  let b0 = (let n = Random.int 100 in
            (candidate (Quote (Integer n)) default_empty_alist = Int n))
  (* Test for booleans *)
  and b1 = (candidate (Quote (Bool true)) default_empty_alist = Boolean true)
  and b2 = (candidate (Quote (Bool false)) default_empty_alist = Boolean false)
  (* Test for characters *)
  and b3 = (candidate (Quote (Char 'a')) default_empty_alist = Character 'a')
  and b4 = (candidate (Quote (Char 'z')) default_empty_alist = Character 'z')
  and b5 = (candidate (Quote (Char '1')) default_empty_alist = Character '1')
  and b6 = (candidate (Quote (Char '9')) default_empty_alist = Character '9')
  and b7 = (candidate (Quote (Char 'A')) default_empty_alist = Character 'A')
  and b8 = (candidate (Quote (Char 'Z')) default_empty_alist = Character 'Z')
  (* Test for strings *)
  and b9 = (candidate (Quote (Str "hello")) default_empty_alist = String "hello")
  and b10 = (candidate (Quote (Str "capstone_2021")) default_empty_alist = String "capstone_2021")
  (* Test for Variables and Recursive Variables *)
  and b11 = (candidate (Quote (Var "x")) default_empty_alist = Symbol "x")
  and b12 = (candidate (Quote (Var_rec ("factorial", 0))) default_empty_alist = Symbol "factorial")
  in b0 && b1 && b2 && b3 && b4 && b5 && b6 && b7 && b8 && b9 && b10 && b11 && b12;;

(* Testing for quotation of non-base values *)
let test_eval_quote_non_base candidate =
  (* Testing for if expressions *)
  let b0 = (candidate (Quote (If (Apply (Var "=",
                                         [Integer 5; Integer 4]),
                                  Char 'Y',
                                  Char 'N'))) default_empty_alist
            = Pair (Symbol "if",
                    Pair (Pair (Symbol "=",
                                Pair (Int 5,
                                      Pair (Int 4,
                                            Null))),
                          Pair (Character 'Y',
                                Pair (Character 'N',
                                      Null)))))
  and b1 = (candidate (Quote (If (Bool true,
                                  Apply (Var "=",
                                         [Integer 5; Integer 5]),
                                  Str "false"))) default_empty_alist
            = Pair (Symbol "if",
                    Pair (Boolean true,
                          Pair (Pair (Symbol "=",
                                      Pair (Int 5,
                                            Pair (Int 5,
                                                  Null))),
                                Pair (String "false",
                                      Null)))))
  (* Test for Let expressions *)
  and b2 = (candidate (Quote (Let ([("five", Integer 5);
                                    ("add_by_two", Lambda_abstraction
                                                     (Args_list ["x"],
                                                      Apply (Var "+",
                                                             [Var "x"; Integer 2])))],
                                   Apply (Var "add_by_two",
                                          [Var "five"])))) default_empty_alist
            = Pair (Symbol "let",
                    Pair (Pair (Pair (Symbol "five",
                                      Pair (Int 5,
                                            Null)),
                                Pair (Pair (Symbol "add_by_two",
                                            Pair (Pair (Symbol "lambda",
                                                        Pair (Pair (Symbol "x",
                                                                    Null),
                                                              Pair (Pair (Symbol "+",
                                                                          Pair (Symbol "x",
                                                                                Pair (Int 2,
                                                                                      Null))),
                                                                    Null))),
                                                  Null)),
                                            Null)),
                                Pair (Pair (Symbol "add_by_two",
                                            Pair (Symbol "five",
                                                  Null)),
                                      Null))))
  and b3 = (candidate (Quote (Let ([("six", Integer 6);
                                    ("seven", Integer 7);
                                    ("compare", Var "<")],
                                   Apply (Var "compare",
                                          [Var "six"; Var "seven"])))) default_empty_alist
            = Pair (Symbol "let",
                    Pair (Pair (Pair (Symbol "six",
                                      Pair (Int 6,
                                            Null)),
                                Pair (Pair (Symbol "seven",
                                            Pair (Int 7,
                                                  Null)),
                                      Pair (Pair (Symbol "compare",
                                                  Pair (Symbol "<",
                                                        Null)),
                                            Null))),
                          Pair (Pair (Symbol "compare",
                                      Pair (Symbol "six",
                                            Pair (Symbol "seven",
                                                  Null))),
                                Null))))
  (* Testing for let-rec expressions *)
  and b4 = (candidate (Quote (Let_rec ([("factorial",
                                         (Args_list ["x"],
                                          If (Apply (Var "=", [Var "x"; Integer 0]),
                                              Integer 1,
                                              Apply (Var "*",
                                                     [Var "x" ;
                                                      Apply (Var_rec ("factorial", 0),
                                                             [Apply (Var "-",
                                                                     [Var "x";
                                                                      Integer 1])])]))))],
                                       Apply (Var_rec ("factorial", 0),
                                              [Var "x"])))) default_empty_alist
            = Pair (Symbol "letrec",
                    Pair (Pair (Pair (Symbol "factorial",
                                      Pair (Pair (Symbol "lambda",
                                                  Pair (Pair (Symbol "x",
                                                              Null),
                                                        Pair (Pair (Symbol "if",
                                                                    Pair (Pair (Symbol "=",
                                                                                Pair (Symbol "x",
                                                                                      Pair (Int 0,
                                                                                            Null))),
                                                                          Pair (Int 1,
                                                                                Pair (Pair (Symbol "*",
                                                                                            Pair (Symbol "x",
                                                                                                  Pair (Pair (Symbol "factorial",
                                                                                                              Pair (Pair (Symbol "-",
                                                                                                                          Pair (Symbol "x",
                                                                                                                                Pair (Int 1,
                                                                                                                                      Null))),
                                                                                                                    Null)),
                                                                                                        Null))),
                                                                                      Null)))),
                                                              Null))),
                                            Null)),
                                Null),
                          Pair (Pair (Symbol "factorial",
                                   Pair (Symbol "x", Null)),
                                Null))))
  (* Testing for implicit applications of lambda expressions *)
  and b5 = (candidate (Quote ((Apply
                                 ((Lambda_abstraction
                                     (Args "x",
                                      Apply (Var "sum_list",
                                             [Var "x"]))),
                                  [Integer 1; Integer 2]))))
              default_empty_alist =
              Pair
                (Pair (Symbol "lambda",
                       Pair (Symbol "x",
                             Pair
                               (Pair (Symbol "sum_list",
                                      Pair (Symbol "x", Null)),
                                Null))),
                 Pair (Int 1,
                       Pair (Int 2,
                             Null))))
  (* Testing for lambda expressions *)
  and b6 = (candidate (Quote (Lambda_abstraction
                                (Args_list ["x"; "y"],
                                 Apply (Var "+",
                                        [Var "x"; Var "y"]))))
              default_empty_alist = Pair (Symbol "lambda",
                                          Pair
                                            (Pair (Symbol "x",
                                                   Pair (Symbol "y", Null)),
                                             Pair
                                               (Pair (Symbol "+",
                                                      Pair (Symbol "x",
                                                            Pair (Symbol "y", Null))),
                                                Null))))
  in b0 && b1 && b2 && b3 && b4 && b5 && b6;;

(* Run the tests. *)
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
assert (test_eval_apply_fixed_arity eval);;
assert (test_eval_apply_variadic eval);;
assert (test_eval_apply_improper eval);;
(test_eval_apply_error eval);;
assert (test_eval_let eval);;
assert (test_eval_let_rec eval);;
assert (test_eval_quote_base eval);;
assert (test_eval_quote_non_base eval);;
