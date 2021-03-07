(* interpreter_cps.ml*)
(* Capstone AY2020/2021 Sem 2 *)
(* Qin Wayne Toh  <toh.qin.wayne@u.yale-nus.edu.sg> *)
(* Version of 16 February 2021 *)

(* Defining the syntax of scheme from BNF *)

(* To do: 1) Implement call-cc, 2) Finish draft on recursion *)

(* FOR INTERPRETER IN CPS *)
open Utils
open Syntax
open Lexing
open Parsing
open Ast_cps
open Unparser_cps
open Interpreter_essentials_cps
   
exception Error of string
exception Not_implemented_yet

let rec aux_map_scheme_proper_list_to_ocaml_list v =
  begin
    match v with
    |Null -> []
    |Pair(v1, v2s) ->
      v1 :: aux_map_scheme_proper_list_to_ocaml_list v2s
    |_ ->
      raise (Error
               (Printf.sprintf 
                    "Error in apply: Not a proper list: %s"
                    (show_exp_val v)))
  end

(* Defining the eval function for quoted expressions *)
let rec eval_quoted_expression e =
  match e with
  | Integer n ->
     Int n
  | Bool b ->
     Boolean b
  | Char c ->
     Character c
  | Str s ->
     String s
  | Var x ->
     Symbol x
  | Var_rec (x, _) ->
     Symbol x
  | If (test, consequent, alternative) ->
     Pair (Symbol "if",
           Pair (eval_quoted_expression test,
                 Pair (eval_quoted_expression consequent,
                       Pair (eval_quoted_expression alternative,
                             Null))))
  | Let (bindings, body) ->
     Pair (Symbol "let",
           Pair (eval_quoted_let_bindings bindings,
                 Pair (eval_quoted_expression body,
                       Null)))
  | Let_rec (bindings, body) ->
     Pair (Symbol "letrec",
           Pair (eval_quoted_letrec_bindings bindings,
                 Pair (eval_quoted_expression body,
                       Null)))
  | Quote e' ->
     Pair (Symbol "quote",
           Pair (eval_quoted_expression e',
                 Null)) 
  | Lambda_abstraction (formals, body) ->
     Pair (Symbol "lambda",
           Pair (eval_quoted_formals formals,
                 Pair (eval_quoted_expression body,
                       Null)))
  | Apply (e0, es) ->
     Pair (eval_quoted_expression e0,
           evlis_quoted_expressions es)
and evlis_quoted_expressions es =
  match es with
  | [] ->
     Null
  | e :: es' ->
     Pair (eval_quoted_expression e,
           evlis_quoted_expressions es')
and eval_quoted_formals formals =
  match formals with
  | Args_list xs ->
     List.fold_right (fun x c -> Pair (Symbol x, c)) xs Null
  | Args_improper_list (xs, x) ->
     List.fold_right (fun x c -> Pair (Symbol x, c)) xs (Symbol x)
  | Args x ->
     Symbol x
and eval_quoted_let_bindings bs =
  match bs with
  | [] ->
     Null
  | (x, e) :: bs ->
     Pair (Pair (Symbol x,
                 Pair (eval_quoted_expression e,
                       Null)),
           eval_quoted_let_bindings bs)
and eval_quoted_letrec_bindings bs =
  match bs with
  | [] ->
     Null
  | (x, (formals, body)) :: bs' ->
     Pair (Pair (Symbol x,
                 Pair (Pair (Symbol "lambda",
                             Pair (eval_quoted_formals formals,
                                   Pair (eval_quoted_expression body,
                                         Null))),
                       Null)),
           eval_quoted_letrec_bindings bs')
    
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
      eval_cps e env (fun v -> evlis_cps es env (fun vs -> myapply v vs k))
      
    |Let (xs, e) ->
      let name_list, exp_list = List.split xs in
      let body = e in
      evlis_cps exp_list env (fun vs -> 
      eval_cps
        body
        (extend_alist_star
           name_list
           vs
           env)
        k)

    |Let_rec (xs, e) ->
      let name_list, lambda_list = List.split xs in
      let ws = (List.map (fun lambda ->
                    begin match lambda with
                    |(Args_list(lambda_formals), lambda_body)
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
                     |(Args(lambda_formal), lambda_body)
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
                     |(Args_improper_list(lambda_formals, lambda_formal), lambda_body)
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
      
    |Lambda_abstraction (formals, body) ->
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
    |Quote e' ->
      k (eval_quoted_expression e')
  end
and evlis_cps (exps : exp list)(env : env)(k : exp_val list -> exp_val): exp_val =
  begin
    match exps with
    |[] ->
      k []
    |exp :: exps' ->
      (* To ensure left to right order evaluation *)
      eval_cps exp env (fun v -> evlis_cps exps' env (fun vs -> k (v :: vs)))
  end
and myapply v vs k =
  begin
    match v with
    |Closure p ->
      p vs k
    |Primitive p ->
      k (p vs)
    |APPLY ->
      applyapply vs k
    |CWCC ->
      applycallcc vs k
     
    |_ ->
      raise (Error
               (Printf.sprintf
                  "Not a procedure: %s"
                  (show_exp_val v)))
  end
and applyapply vs k =
  begin
    match vs with
    |v1 :: v2 :: [] ->
      let vs = aux_map_scheme_proper_list_to_ocaml_list v2 in
      begin
        match v1 with
        |Closure p ->
          p vs k
        |Primitive p ->
          k (p vs)
        |CWCC ->
          applycallcc vs k
        |APPLY ->
          applyapply vs k
        |_ ->
          raise (Error
                   (Printf.sprintf
                      "Error in apply: Not a procedure: %s"
                      (show_exp_val v1)))
      end
    |_ ->
      raise (Error
               (Printf.sprintf 
                  "Incorrect argument count in call (apply %s)"
                  (show_list show_exp_val vs)))
  end
and applycallcc vs k =
  (* the _ denotes the discarded continuation *)
  begin
    match vs with
    |v :: [] ->
      begin
        match v with
        |Closure p ->
          p [Closure (fun ws _ ->
                 begin
                   match ws with
                   |w :: [] ->
                     k w
                   |_ ->
                     raise (Error
                              (Printf.sprintf
                                 "Incorrect argument count to captured continuation %s"
                                 (show_list show_exp_val ws)))
                 end)] k
        |Primitive p ->
          k (p [Closure (fun ws _ ->
                    begin
                      match ws with
                      |w :: [] ->
                        k w
                      |_ ->
                        raise (Error
                                 (Printf.sprintf
                                    "Incorrect argument count to captured continuation %s"
                                    (show_list show_exp_val ws)))
                    end)])
         
        |CWCC ->
          let ck = (Closure (fun ws _ ->
                        begin
                          match ws with
                          |w :: [] ->
                            k w
                          |_ ->
                            raise (Error
                                     (Printf.sprintf
                                        "Incorrect argument count to captured continuation %s"
                                        (show_list show_exp_val ws)))
                        end)) in
          myapply CWCC [ck] k
          
          
        |_ ->
          raise (Error
                   (Printf.sprintf
                      "Error in call/cc: Not a procedure: %s"
                      (show_exp_val v)))
         
      end
    |_ -> raise (Error
                   (Printf.sprintf
                      "Incorrect argument count to call (call/cc %s)"
                      (show_list show_exp_val vs)))
  end

let expression_matcher toplevelform =
  begin
    match toplevelform with
    |Exp e -> e
    |Define (_, e) ->
      e
  end

let get_final_parsed_exp s =
  expression_matcher (Parse_Scheme_toplevel_form.parse (lex s))

let interpreter_cps exp =
  eval_cps exp default_empty_alist (fun v -> v)

let interpreter_cps_on_scheme_input s =
  eval_cps (get_final_parsed_exp s) default_empty_alist (fun v -> v)
