open Ast_cps
open Interpreter_essentials_cps

exception Not_implemented_yet
                 
(* Testing the environment and its pre-defined functions *)

(* Testing for error: If the correct error is raised, test returns unit meaning that there is no exception raised. *)


       
let prepopulated_env = [("test", Int 10);
                        ("var", String "var");
                        ("env", Boolean false)];; 

let test_extend_alist candidate =
  let b0 = ((candidate
               "x"
               (Int 5)
               empty_alist) = [("x", Int 5)])
  and b1 = ((candidate
               "x"
               (Boolean true)
               empty_alist) = [("x", Boolean true)])
  and b2 = ((candidate
               "x"
               (String "hello world")
               empty_alist) = [("x", String "hello world")])
  and b3 = ((candidate
               "x"
               (Character '9')
               empty_alist) = [("x", Character '9')])
  and b4 = ((candidate
               "x"
               (Pair (Int 5, 
                      Int 6))
               empty_alist) = [("x", Pair (Int 5,
                                          Int 6))])
  and b5 = ((candidate
               "x"
               (Int 5)
               prepopulated_env) = [("x", Int 5);
                                    ("test", Int 10);
                                    ("var", String "var");
                                    ("env", Boolean false)])
  and b6 = ((candidate
               "x"
               (Boolean true)
               prepopulated_env) = [("x", Boolean true);
                                    ("test", Int 10);
                                    ("var", String "var");
                                    ("env", Boolean false)])
  and b7 = ((candidate
               "x"
               (String "hello world")
               prepopulated_env) = [("x", String "hello world");
                                    ("test", Int 10);
                                    ("var", String "var");
                                    ("env", Boolean false)])
  and b8 = ((candidate
               "x"
               (Character '9')
               prepopulated_env) = [("x", Character '9');
                                    ("test", Int 10);
                                    ("var", String "var");
                                    ("env", Boolean false)])
  and b9 = ((candidate
               "x"
               (Pair (Int 5,
                      Int 6))
               prepopulated_env) = [("x", Pair (Int 5, Int 6));
                                    ("test", Int 10);
                                    ("var", String "var");
                                    ("env", Boolean false)])
  in b0 && b1 && b2 && b3 && b4 && b5 && b6 && b7 && b8 && b9;; 

assert (test_extend_alist extend_alist);; 

let test_extend_alist_star candidate =
  let b0 = ((candidate
               ["x"; "y"; "z"]
               [Int 5; Int 6; Int 7]
               empty_alist) = ([("x", Int 5);
                               ("y", Int 6);
                               ("z", Int 7)]))
  and b1 = ((candidate
               ["x"; "y"]
               [Boolean false; Character 'a']
               empty_alist) = ([("x", Boolean false);
                               ("y", Character 'a')]))
  and b2 = ((candidate
               ["x"]
               [Pair (Int 5,
                      Character '9')]
               empty_alist) = ([("x", Pair (Int 5,
                                           Character '9'))]))
  and b3 = ((candidate
               ["x"; "y"; "z"]
               [Int 5; Int 6; Int 7]
               prepopulated_env) = [("x", Int 5);
                                    ("y", Int 6);
                                    ("z", Int 7);
                                    ("test", Int 10);
                                    ("var", String "var");
                                    ("env", Boolean false)])
  and b4 = ((candidate
               ["x"; "y"]
               [Boolean false; Character 'a']
               prepopulated_env) = [("x", Boolean false);
                                    ("y", Character 'a');
                                    ("test", Int 10);
                                    ("var", String "var");
                                    ("env", Boolean false)])
  and b5 = ((candidate
               ["x"]
               [Pair (Int 5,
                      Character '9')]
               prepopulated_env) = [("x", Pair (Int 5,
                                                Character '9'));
                                    
                                    ("test", Int 10);
                                    ("var", String "var");
                                    ("env", Boolean false)])
  in b0 && b1 && b2 && b3 && b4 && b5;; 

let test_extend_alist_star_error candidate =
  let b0 = (try ignore (candidate
                          ["x"; "y"]
                          [Int 5; Int 6; Int 9]
                          empty_alist);
                failwith
                  "Error not occurring" with Interpreter_essentials_cps.Error ("Arity mismatch, [\"x\"; \"y\"]  [5; 6; 9]") -> ())
  and b1 = (try ignore (candidate
                          ["x"; "y"; "z"]
                          [Int 5; Character 'c']
                          empty_alist);
                failwith
                  "Error not occurring" with Interpreter_essentials_cps.Error ("Arity mismatch,  [\"x\"; \"y\"; \"z\"]  [5; 'c']") -> ())
  and b2 = (try ignore (candidate
                          ["x"; "y"]
                          [Int 5; Int 6; Int 7; Int 8]
                          empty_alist);
                failwith
                  "Error not occurring" with Interpreter_essentials_cps.Error ("Arity mismatch, [\"x\"; \"y\"]  [5; 6; 7; 8]") -> ())
  and b3 = (try ignore (candidate
                          ["x"; "y"; "z"; "var"]
                          [Int 5; Character 'c']
                          empty_alist);
                failwith
                  "Error not occurring" with Interpreter_essentials_cps.Error ("Arity mismatch,  [\"x\"; \"y\"; \"z\"; \"var\"]  [5; 'c']") -> ())
  and b4 = (try ignore (candidate
                          ["x"; "y"]
                          [Int 5; Int 6; Int 7]
                          prepopulated_env);
                failwith
                  "Error not occurring" with Interpreter_essentials_cps.Error ("Arity mismatch, [\"x\"; \"y\"]  [5; 6; 7]") -> ())
  and b5 = (try ignore (candidate
                          ["x"; "y"; "z"]
                          [Int 5; Character 'c']
                          prepopulated_env);
                failwith
                  "Error not occurring" with Interpreter_essentials_cps.Error ("Arity mismatch,  [\"x\"; \"y\"; \"z\"]  [5; 'c']") -> ())
  in b0; b1; b2; b3; b4; b5;;

assert (test_extend_alist_star extend_alist_star);; 
(test_extend_alist_star_error extend_alist_star);;   

(* Testing the lookup function *)

let test_lookup candidate =
  let b0 = ((candidate "test" prepopulated_env) = Int 10)
  and b1 = ((candidate "var" prepopulated_env) = String "var")
  and b2 = ((candidate "env" prepopulated_env) = Boolean false)
  and b3 = ((candidate "x" [("x", Character 'x')]) = Character 'x')
  and b4 = ((candidate "y" [("y", String "first");
                            ("y", String "second");
                            ("x", Boolean false)]) = String "first")
  in b0 && b1 && b2 && b3 && b4;;

let test_lookup_error candidate =
  let b0 = (try ignore (candidate "test" empty_alist);
                failwith "bad lookup" with (Interpreter_essentials_cps.Lookup_not_found "test") -> ())
  and b1 = (try ignore (candidate "var" empty_alist);
                failwith "bad lookup" with (Interpreter_essentials_cps.Lookup_not_found "var") -> ())
  and b2 = (try ignore (candidate "env" empty_alist);
                failwith "bad lookup" with (Interpreter_essentials_cps.Lookup_not_found "env") -> ())
  and b3 = (try ignore (candidate "test" [("x", Int 5);
                                          ("y", Boolean false)]);
                failwith "bad lookup" with (Interpreter_essentials_cps.Lookup_not_found "test") -> ())
  and b4 = (try ignore (candidate "var" [("x", Int 5);
                                         ("y", Boolean false)]);
                failwith "bad lookup" with (Interpreter_essentials_cps.Lookup_not_found "var") -> ())
  and b5 = (try ignore (candidate "env" [("x", Int 5);
                                         ("y", Boolean false)]);
                failwith "bad lookup" with (Interpreter_essentials_cps.Lookup_not_found "env") -> ())
  in b0; b1; b2; b3; b4; b5;;

assert (test_lookup lookup);;
(test_lookup_error lookup);;
