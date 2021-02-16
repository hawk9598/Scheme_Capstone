(* interpreter_cps.ml*)
(* Capstone AY2020/2021 Sem 2 *)
(* Qin Wayne Toh  <toh.qin.wayne@u.yale-nus.edu.sg> *)
(* Version of 16 February 2021 *)

(* Defining the syntax of scheme from BNF *)

(* To do: 1) Implement call-cc, 2) Finish draft on recursion *)

(* FOR INTERPRETER IN CPS *)

open Ast_cps
open Unparser_cps
open Interpreter_essentials_cps
   
exception Error of string
exception Not_implemented_yet


(* Defining the evaluation function/ interpreter in CPS. *)
        
let rec eval_cps (exp : exp) (env: env)(k:  exp_val -> exp_val): exp_val =
  (* For debugging *)
  (* Printf.printf "%s\n" (show_exp exp); *)
  begin
    match exp with
    |Integer i ->
      k (Int i) 
    |Bool b ->
      k (Boolean b)
    |Var x ->
      k (lookup x env)
    |Var_rec (x, i) ->
      k (lookup_rec x i env)
    |Str s ->
      k (String s)
    |Char c ->
      k (Character c)
    |If (exp1, exp2, exp3) -> 
      eval_cps exp1 env (fun v ->           
        match v with
        |Boolean false ->
          eval_cps exp3 env k
        |_ ->
          eval_cps exp2 env k)
    |Apply (e, es) ->
      (* Left to right evaluation order *)
      eval_cps e env (fun v -> evlis_cps es env (fun vs ->
      begin
        match v with
        |Closure p ->
          p vs k
        |Primitive p ->
          k (p vs)
        (* Just need to finish these 2 clauses, add apply and ccc inside initial environment *)
        |APPLY ->
          failwith "unimplemented"
        |CCC ->
          failwith "unimplemented"
        |_ ->
          raise (Error
                   (Printf.sprintf
                      "Not a procedure: %s"
                      (show_exp_val v)))
      end))
    |Let (_) ->
      failwith "unimplemented"
      (*
      let name_list, exp_list = List.split xs in
      let exp_val_list = evlis_cps exp_list env k in
      let body = e in
      eval_cps
        body
        (extend_alist_star
           name_list
           exp_val_list
           env)
        k *)

    |Let_rec (xs, e) ->
      let name_list, lambda_list = List.split xs in
      let ws = (List.map (fun lambda ->
                    begin match lambda with
                    |Lambda(Args_list(lambda_formals), lambda_body)
                     ->
                      (fun (Recur_star ws) vs k ->
                        eval_cps
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
                                env))
                          k)
                     |Lambda(Args(lambda_formal), lambda_body)
                     ->
                      (fun (Recur_star ws) vs k ->
                        eval_cps
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
                                env))
                          k)
                     |Lambda(Args_improper_list(lambda_formals, lambda_formal), lambda_body)
                      ->
                       (fun (Recur_star ws) vs k ->
                         let xs = lambda_formals in
                         let x = lambda_formal in
                         let (vs', v) = aux_map_ocaml_list_to_scheme_improper_list vs xs in
                         eval_cps
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
                                    env)))
                           k)
                    end)
                  lambda_list)
      in
      eval_cps e
        (extend_alist_star
           name_list
           (List.init
              (List.length name_list)
              (fun _ -> Recursive_closure(Recur_star ws)))
           env)
        k
      
    |Lambda_abstraction (Lambda(formals, body)) ->
      k
      (begin
        match formals with
        |Args_list xs ->
          Closure 
            (fun vs k ->
              eval_cps
                body
                (extend_alist_star xs vs env)
                k)
        |Args x ->
          Closure
            (fun vs k->
              eval_cps
                body
                (extend_alist x (aux_map_ocaml_list_to_scheme_proper_list vs) env)
                k)
        |Args_improper_list (xs, x) ->
          Closure
            (fun vs k->
              let (vs', v) = aux_map_ocaml_list_to_scheme_improper_list vs xs in
              eval_cps
                body
                (extend_alist x v (extend_alist_star xs vs' env))
                k)
      end)                          
  end
and evlis_cps (exps : exp list)(env : env)(k : exp_val list -> exp_val): exp_val =
  begin
    match exps with
    |[] ->
      k []
    |exp :: exps' ->
      (* To ensure left to right order evaluation *)
      eval_cps exp env (fun v -> evlis_cps exps' env (fun vs -> k (v :: vs)))
  end;;
