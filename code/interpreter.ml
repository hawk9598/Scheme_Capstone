(* scheme_interpreter_in_ocaml.ml*)
(* Capstone AY2020/2021 Sem 1 *)
(* Qin Wayne Toh  <toh.qin.wayne@u.yale-nus.edu.sg> *)
(* Version of 23 October 2020 *)

(* Defining the syntax of scheme from BNF *)

(* cons, car, cdr, plus, etc as primitive operators. Put lots of things inside the initial environment *)

(* open Primitives *)
open Ast
open Unparser
open Interpreter_essentials 
   
exception Error of string
exception Not_implemented_yet
        
(* Defining the evaluation function *)
let rec eval (exp : exp) (env: env): exp_val =
  begin
    match exp with
    |Integer i ->
      Int i
    |Bool b ->
      Boolean b
    |Var x ->
      lookup x env
    |Str s ->
      String s
    |Char c ->
      Character c
    |If (exp1, exp2, exp3) ->
      begin
        match (eval exp1 env) with
        |Boolean false ->
          eval exp3 env
        |_ ->
          eval exp2 env
      end
    |Apply (e, es) ->
      (* Left to right evaluation order *)
      let v = eval e env in
      let vs = evlis es env in
      begin
        match v with
        |Closure p ->
          p vs
        |Primitive p ->
          p vs
        |_ ->
          raise (Error
                   (Printf.sprintf
                      "Not a procedure: %s"
                      (show_exp_val v)))
      end
    |Let (_, _) ->
      raise Not_implemented_yet
    |Let_rec (_, _) ->
      raise Not_implemented_yet
    |Lambda_abstraction (Lambda(formals, body)) ->
      begin
        match formals with
        |Args_list xs ->
          Closure
            (fun vs ->
              eval
                body
                (extend_alist_star xs vs env))
      end                          
  end
and evlis (exps : exp list)(env : env): exp_val list =
  begin
    match exps with
    |[] ->
      []
    |exp :: exps' ->
      (* To ensure left to right order evaluation *)
      let v = eval exp env
      in v :: evlis exps' env
  end;;

