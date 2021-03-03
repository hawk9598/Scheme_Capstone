(* about_evaluating_a_quote_expression.ml *)
(* Olivier Danvy <danvy@yale-nus.edu.sg> *)
(* Singapore, Mon 02 Mar 2021 *)

(* ********** *)

module Quotations =
  struct

    type name = string
                  
    type exp =
      |Integer of int (* Integer *)
      |Bool of bool (* Boolean *)
      |Char of char (* Character *)
      |Str of string (* String *)
      |Var of name (* Variable *) (* Names denoting predefined procedures *)
      |Var_rec of name * int (* Variable recursive *)
      |If of exp * exp * exp (* If expression *)
      |Let of (name * exp) list * exp (* Let expression *)
      |Let_rec of (name * (lambda_formals * exp)) list * exp (* Let rec expression *)
      |Quote of exp
      |Lambda_abstraction of lambda_formals * exp (* Lambda abstraction *)
      |Apply of exp * exp list (* Apply expression *)
     and lambda_formals =
       |Args_list of name list
       (* For (lambda (x y z. extras) ... ) *)
       |Args_improper_list of name list * name
       (* For variadic procedures *)
       |Args of name

    type exp_val =
      |Int of int
      |Boolean of bool
      |String of string
      |Character of char
      |Pair of exp_val * exp_val (* decide to make pairs immutable for now *)
      |Closure of (exp_val list -> exp_val)
      |Primitive of (exp_val list -> exp_val)(* string * (exp_val -> exp_val) *)
      |Null (* empty list *)
      |Recursive_closure of recur_star
      |Symbol of name    (* <--- new, and necessary to represent variables *)
     and recur_star = 
       |Recur_star of (recur_star -> exp_val list -> exp_val) list

    exception Not_implemented;;

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

    let eval e =
      match e with
      | Quote e' ->
         eval_quoted_expression e'
      | _ ->
         raise Not_implemented;;
  end;;

(* ********** *)

(*
   # Parse_Scheme_toplevel_form.parse (lex "(letrec ((f0 (lambda (x0) (f1 x0))) (f1 (lambda (x1) (f0 x1)))) (f0 f1 x))");;
   - : Wayne.top_level_form =
   Wayne.Exp
    (Wayne.Let_rec
      ([("f0",
         (Wayne.Args_list ["x0"],
          Wayne.Apply (Wayne.Var_rec ("f1", 1), [Wayne.Var "x0"])));
        ("f1",
         (Wayne.Args_list ["x1"],
          Wayne.Apply (Wayne.Var_rec ("f0", 0), [Wayne.Var "x1"])))],
      Wayne.Apply (Wayne.Var_rec ("f0", 0),
       [Wayne.Var_rec ("f1", 1); Wayne.Var "x"])))
   # Quotations.eval (Quotations.Quote
    (Quotations.Let_rec
      ([("f0",
         (Quotations.Args_list ["x0"],
          Quotations.Apply (Quotations.Var_rec ("f1", 1), [Quotations.Var "x0"])));
        ("f1",
         (Quotations.Args_list ["x1"],
          Quotations.Apply (Quotations.Var_rec ("f0", 0), [Quotations.Var "x1"])))],
      Quotations.Apply (Quotations.Var_rec ("f0", 0),
       [Quotations.Var_rec ("f1", 1); Quotations.Var "x"])))) [];;
                     - : Quotations.exp_val =
   Quotations.Pair (Quotations.Symbol "letrec",
    Quotations.Pair
     (Quotations.Pair
       (Quotations.Pair (Quotations.Symbol "f0",
         Quotations.Pair
          (Quotations.Pair (Quotations.Symbol "lambda",
            Quotations.Pair
             (Quotations.Pair (Quotations.Symbol "x0", Quotations.Null),
             Quotations.Pair
              (Quotations.Pair (Quotations.Symbol "f1",
                Quotations.Pair (Quotations.Symbol "x0", Quotations.Null)),
              Quotations.Null))),
          Quotations.Null)),
       Quotations.Pair
        (Quotations.Pair (Quotations.Symbol "f1",
          Quotations.Pair
           (Quotations.Pair (Quotations.Symbol "lambda",
             Quotations.Pair
              (Quotations.Pair (Quotations.Symbol "x1", Quotations.Null),
              Quotations.Pair
               (Quotations.Pair (Quotations.Symbol "f0",
                 Quotations.Pair (Quotations.Symbol "x1", Quotations.Null)),
               Quotations.Null))),
           Quotations.Null)),
        Quotations.Null)),
     Quotations.Pair
      (Quotations.Pair (Quotations.Symbol "f0",
        Quotations.Pair (Quotations.Symbol "f1",
         Quotations.Pair (Quotations.Symbol "x", Quotations.Null))),
      Quotations.Null)))
   # 
*)

(* ********** *)

(* end of about_evaluating_a_quote_expression.ml *)
