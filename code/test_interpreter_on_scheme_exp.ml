open Ast
open Interpreter
open Interpreter_essentials
open Utils
open Syntax
open Lexing
open Parsing

exception Not_implemented_yet
exception Not_expected
        
let expression_matcher toplevelform =
  begin
    match toplevelform with
    |Exp e -> e
    |Define (_, e) ->
      e
  end

let get_final_parsed_exp s =
  expression_matcher (Parse_Scheme_toplevel_form.parse (lex s))
        
(* Testing the eval function on parsed Scheme expressions *)
        
let test_eval_Scheme_base_values candidate =
  (* Test on integers *)
  let b0 = (candidate (get_final_parsed_exp "1")
              default_empty_alist = Int 1)
  and b1 = (candidate (get_final_parsed_exp "1000")
              default_empty_alist = Int 1000)
  and b2 = (candidate (get_final_parsed_exp "-1000")
              default_empty_alist = Int (-1000))
  and b3 = (candidate (get_final_parsed_exp "0")
              default_empty_alist = Int 0)
  (* Test on booleans *)
  and b4 = (candidate (get_final_parsed_exp "#f")
              default_empty_alist = Boolean false)
  and b5 = (candidate (get_final_parsed_exp "#t")
              default_empty_alist = Boolean true)
  (* Test on characters *)
  and b6 = (candidate (get_final_parsed_exp "#\\A")
              default_empty_alist = Character 'A')
  and b7 = (candidate (get_final_parsed_exp "#\\z")
              default_empty_alist = Character 'z')
  and b8 = (candidate (get_final_parsed_exp "#\\0")
              default_empty_alist = Character '0')
  and b9 = (candidate (get_final_parsed_exp "#\\9")
              default_empty_alist = Character '9') 
  and b10 = (candidate (get_final_parsed_exp "\"string\"")
               default_empty_alist = String "\"string\"")
  and b11 = (candidate (get_final_parsed_exp "\"hawk9598\"")
               default_empty_alist = String "\"hawk9598\"")
  and b12 = (candidate (get_final_parsed_exp "\"capstone_2021!\"")
               default_empty_alist = String "\"capstone_2021!\"")
  in b0 && b1 && b2 && b3 && b4 && b5 && b6 && b7 && b8 && b9 && b10 && b11 && b12;;


let test_eval_Scheme_non_base_values candidate =
  (* Test on if else expressions *)
  let b0 = (candidate (get_final_parsed_exp "(if #t #\\Y 0)")
              default_empty_alist  = Character 'Y')
  and b1 = (candidate (get_final_parsed_exp "(if #f -100 #\\n)")
              default_empty_alist = Character 'n')
  and b2 = (candidate (get_final_parsed_exp "(if (= 5 5) -100 #\\n)")
              default_empty_alist = Int (-100))
  and b3 = (candidate (get_final_parsed_exp "(if #t (+ 5 3 1 2 4) -10)")
              default_empty_alist = Int 15)
  and b4 = (candidate (get_final_parsed_exp "(if -5 #\\Y (+ 1 2 3))")
              default_empty_alist = Character 'Y')
  and b5 = (candidate (get_final_parsed_exp "(if (= 5 1) (+ 1 3) (* 4 2 3))")
              default_empty_alist = Int 24)
  (* Test on expressions that apply Variables *)
  and b6 = (candidate (get_final_parsed_exp "(+ 5 3 1 2 40)")
              default_empty_alist = Int 51)
  and b7 = (candidate (get_final_parsed_exp "(integer? 5)")
              default_empty_alist = Boolean true)
  and b8 = (candidate (get_final_parsed_exp "(car '(1 2))")
              default_empty_alist = Int 1)
  and b9 = (candidate (get_final_parsed_exp "(car (cdr '(1 2)))")
              default_empty_alist = Int 2)
  and b10 = (candidate (get_final_parsed_exp "(/ 1000 200 5)")
               default_empty_alist = Int 1)
  and b11 = (candidate (get_final_parsed_exp "(expt 2 5)")
               default_empty_alist = Int 32)
  and b12 = (candidate (get_final_parsed_exp "(char? #\\Y)")
               default_empty_alist = Boolean true)
  and b13 = (candidate (get_final_parsed_exp "(string-ref \"string\" 1)")
               default_empty_alist = Character 's')
  (* Test on let expressions *)
  and b14 = (candidate (get_final_parsed_exp "(let ((five 5)) (+ five 3))")
               default_empty_alist = Int 8)
  in b0 && b1 && b2 && b3 && b4 && b5 && b6 && b7 && b8 && b9 && b10 && b11 && b12 && b13 && b14;;
  
assert (test_eval_Scheme_base_values eval);;
assert (test_eval_Scheme_non_base_values eval);;
