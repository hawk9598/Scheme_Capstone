(* scheme_interpreter_in_ocaml.ml*)
(* Capstone AY2020/2021 Sem 2 *)
(* Qin Wayne Toh  <toh.qin.wayne@u.yale-nus.edu.sg> *)
(* Version of 6 February 2021 *)

(* Defining the syntax of scheme from BNF *)

open Ast
open Unparser
open Interpreter_essentials 
   
exception Error of string
exception Not_implemented_yet

(* TO DO: Implement call-cc and apply for our interpreter function *)
        
(* Defining the evaluation function/ interpreter. *)
        
let rec eval (exp : exp) (env: env): exp_val =
  (* For debugging *)
  (* Printf.printf "%s\n" (show_exp exp); *)
  begin
    match exp with
    |Integer i ->
      Int i 
    |Bool b ->
      Boolean b
    |Var x ->
      lookup x env
    |Var_rec (x, i) ->
      lookup_rec x i env
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
        |Recursive_closure f ->
          f (Recursive_closure f) vs
        |_ ->
          raise (Error
                   (Printf.sprintf
                      "Not a procedure: %s"
                      (show_exp_val v)))
      end
    |Let (_, _) ->
      raise Not_implemented_yet

    |Let_rec (xs, e) ->
      let name_list, lambda_list = List.split xs in
      let ws = (List.map (fun lambda ->
                    begin match lambda with
                    |Lambda(Args_list(lambda_formals), lambda_body)
                     ->
                      (fun (Recur_star ws) vs ->
                        eval
                          lambda_body
                          (extend_alist_star
                             lambda_formals
                             vs
                             (extend_alist_star
                                name_list
                                (List.init
                                   (List.length name_list)
                                   (fun _ ->
                                     (Recursive_closure_non_unary
                                        (Recur_star ws))))
                                env)))
                    end)
                  lambda_list)
      in
      eval e
        (extend_alist_star
           name_list
           (List.init
              (List.length name_list)
              (fun _ -> Recursive_closure_non_unary (Recur_star ws)))
           env)
            
    |Let_rec_unary(x, e) ->
      let name, lambda = x in  
      let lambda_args, lambda_body =
        begin match lambda with
        |Lambda (args, exp) ->
          args, exp
        end
      in
      let lambda_formals =
        begin match lambda_args with
        |Args_list xs ->
          xs
        end
      in
      eval e (extend_alist
                name
                (Recursive_closure
                   (fun closure ->
                     fun vs ->
                     eval
                       lambda_body 
                       (extend_alist_star
                          lambda_formals
                          vs
                          (extend_alist name closure env))))
                env)             
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
