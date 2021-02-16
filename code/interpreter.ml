(* interpreter.ml*)
(* Capstone AY2020/2021 Sem 2 *)
(* Qin Wayne Toh  <toh.qin.wayne@u.yale-nus.edu.sg> *)
(* Version of 16 February 2021 *)

(* Defining the syntax of scheme from BNF *)

(* To do: 1) Finish draft on recursion *)

open Ast
open Interpreter_essentials 
open Unparser
    
exception Error of string
exception Not_implemented_yet
                          
(* Defining the evaluation function/ interpreter. *)
        
let rec eval exp env =
  (* (exp : exp) (env: env): exp_val*)
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
        |_ ->
          raise (Error
                   (Printf.sprintf
                      "Not a procedure: %s"
                      (show_exp_val v)))
      end
    |Let (xs, e) ->
      let name_list, exp_list = List.split xs in
      let exp_val_list = evlis exp_list env in
      let body = e in
      eval
        body
        (extend_alist_star
           name_list
           exp_val_list
           env)
      

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
                                     (Recursive_closure
                                        (Recur_star ws))))
                                env)))
                     |Lambda(Args(lambda_formal), lambda_body)
                     ->
                      (fun (Recur_star ws) vs ->
                        eval
                          lambda_body
                          (extend_alist
                             lambda_formal
                             (aux_map_ocaml_list_to_scheme_proper_list vs)
                             (extend_alist_star
                                name_list
                                (List.init
                                   (List.length name_list)
                                   (fun _ ->
                                     (Recursive_closure
                                        (Recur_star ws))))
                                env)))
                     |Lambda(Args_improper_list(lambda_formals, lambda_formal), lambda_body)
                      ->
                       (fun (Recur_star ws) vs ->
                         let xs = lambda_formals in
                         let x = lambda_formal in
                         let (vs', v) = aux_map_ocaml_list_to_scheme_improper_list vs xs in
                         eval
                           lambda_body
                           (extend_alist_star
                              xs
                              vs'
                              (extend_alist_star
                                 name_list
                                 (List.init
                                    (List.length name_list)
                                    (fun _ ->
                                      (Recursive_closure
                                         (Recur_star ws))))
                                 (extend_alist
                                    x
                                    v
                                    env))))
                    end)
                  lambda_list)
      in
      eval e
        (extend_alist_star
           name_list
           (List.init
              (List.length name_list)
              (fun _ -> Recursive_closure (Recur_star ws)))
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
        |Args x ->
          Closure
            (fun vs ->
              eval
                body
                (extend_alist x (aux_map_ocaml_list_to_scheme_proper_list vs) env))
        |Args_improper_list (xs, x) ->
          Closure
            (fun vs ->
              let (vs', v) = aux_map_ocaml_list_to_scheme_improper_list vs xs in
              eval
                body
                (extend_alist x v (extend_alist_star xs vs' env)))
      end                          
  end
and evlis exps env =
  (*(exps : exp list)(env : env): exp_val list *)
  begin
    match exps with
    |[] ->
      []
    |exp :: exps' ->
      (* To ensure left to right order evaluation *)
      let v = eval exp env
      in v :: evlis exps' env
  end;;
