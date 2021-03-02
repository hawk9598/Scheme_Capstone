(* parsing.ml *)
(* Olivier Danvy <danvy@yale-nus.edu.sg> *)
(* Singapore, Sun 28 Feb 2021 & Mon 02 Mar 2021 *)

(* ********** *)

module Wayne =
  struct
    type name = string
                  
    type arg_name = Arg of name
                             
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
      |Quote of exp (* <--- *)
      |Lambda_abstraction of lambda_formals * exp (* Lambda abstraction *)
      |Apply of exp * exp list (* Apply expression *)
     and lambda_formals =
       |Args_list of name list
       (* For (lambda (x y z. extras) ... ) *)
       |Args_improper_list of name list * name
       (* For variadic procedures *)
       |Args of name

    type top_level_form =
      |Define of name * exp
      |Exp of exp

    type prog =
      |Prog of top_level_form list
  end;;

(* ********** *)

module Olivier =
  struct
    type exp =
      | True
      | False
      | Int of int
      | Char of char
      | String of string
      | Name of string
      | Quote of exp
      | If of exp * exp * exp
      | Lambda of formals * exp
      | Let of (string * exp) list * exp
      | Letrec of (string * (formals * exp)) list * exp
      | Apply of exp * exp list
     and formals =
       | Single_formal of string
       | Proper_formals of string list
       | Improper_formals of string list * string;;

    type toplevel_form =
      | Definition of string * exp
      | Expression of exp;;

    (* ********** *)

    let silent_flag = ref true;;

    exception Imparsable of string;;

    exception Not_implemented_yet;;

    let is_a_keyword s =
      match s with
      | "if" | "lambda" | "letrec" ->
         true
      | _ ->
         false;;

    let list_member x xs_given =
      let rec loop xs =
        match xs with
        | [] ->
           false
        | x' :: xs' ->
           if x = x'
           then true
           else loop xs'
      in loop xs_given;;

    let rec top ts =
      match ts with
      | [] ->
         raise (Imparsable "empty list of tokens")
      | TRUE :: ts' ->
         (True, ts')
      | FALSE :: ts' ->
         (False, ts')
      | INT n :: ts' ->
         (Int n, ts')
      | CHAR c :: ts' ->
         (Char c, ts')
      | STRING s :: ts' ->
         (String s, ts')
      | ATOM s :: ts' ->
         if is_a_keyword s
         then raise (Imparsable "keyword out of place")
         else (Name s, ts')
      | DOT :: ts' ->
         raise (Imparsable "dot out of place")
      | QUOTE :: ts' ->
         let (e, ts'') = top ts'
         in (Quote e, ts'')
      | RP :: ts' ->
         raise (Imparsable "close parenthesis out of place")
      | LP :: ts' ->
         visit_LP ts'
    and visit_LP ts =
      match ts with
      | [] ->
         raise (Imparsable "missing close parenthesis")
      | t :: ts' ->
         visit_LP' t ts'
    and visit_LP' t ts =
      match t with
      | DOT ->
         raise (Imparsable "dot out of place")
      | QUOTE ->
         let (e, ts') = top ts
         in (Quote e, ts')
      | ATOM "if" ->
         let (test, ts') = top ts
         in let (consequent, ts'') = top ts'
            in let (alternative, ts''') = top ts''
               in (match ts''' with
                   | RP :: ts''''' ->
                      (If (test, consequent, alternative), ts''''')
                   | _ ->
                      raise (Imparsable "if-expression"))
      | ATOM "lambda" ->
         visit_LAMBDA ts
      | ATOM "let" ->
         visit_LET ts
      | ATOM "letrec" ->
         visit_LETREC ts
      | _ ->
         let (e, ts') = top (t :: ts)
         in let (es, ts'') = visit_actuals ts'
            in (Apply (e, es), ts'')
    and visit_actuals ts =
      match ts with
      | [] ->
         raise (Imparsable "incomplete application")
      | RP :: ts' ->
         ([], ts')
      | _ ->
         let (e, ts'') = top ts
         in let (es, ts''') = visit_actuals ts''
            in (e :: es, ts''')
    and visit_LAMBDA ts =
      let (formals, ts') = visit_FORMALS ts
      in let (body, ts'') = top ts'
         in match ts'' with
            | [] ->
               raise (Imparsable "incomplete lambda-abstraction")
            | RP :: ts''' ->
               (Lambda (formals, body), ts''')
            | _ ->
               raise (Imparsable "overflowing lambda-abstraction")
    and visit_FORMALS ts =
      let () = if !silent_flag
               then ()
               else Printf.printf "visit_FORMALS %s\n" (unparse_lexemes ts) in
      match ts with
      | [] ->
         raise (Imparsable "incomplete lambda-abstraction")
      | ATOM s :: ts' ->
         if is_a_keyword s
         then raise (Imparsable "keyword as formal in lambda-abstraction")
         else (Single_formal s, ts')
      | LP :: ts' ->
         let rec visit_formalss ts a =
           let () = if !silent_flag
                    then ()
                    else Printf.printf "visit_formalss %s\n" (unparse_lexemes ts) in
           match ts with
           | [] ->
              raise (Imparsable "incomplete formals in lambda-abstraction")
           | RP :: ts' ->
              (Proper_formals (List.rev a), ts')
           | ATOM s :: ts' ->
              if is_a_keyword s
              then raise (Imparsable "keyword as formal in lambda-abstraction")
              else if list_member s a
              then raise (Imparsable "indistinct proper formals in lambda-abstraction")
              else visit_formalss ts' (s :: a)
           | DOT :: ATOM s :: RP :: ts'' ->
              if is_a_keyword s
              then raise (Imparsable "keyword as last improper formal in lambda-abstraction")
              else if list_member s a
              then raise (Imparsable "indistinct proper formals in lambda-abstraction")
              else (Improper_formals (List.rev a, s), ts'')
           | _ ->
              raise (Imparsable "informal formals in lambda-abstraction")
         in visit_formalss ts' []
      | _ ->
         raise (Imparsable "illegal formals in lambda-abstraction")
    and visit_LET ts =
      let () = if !silent_flag
               then ()
               else Printf.printf "visit_LET %s\n" (unparse_lexemes ts) in
      match ts with
      | [] ->
         raise (Imparsable "incomplete let expression")
      | LP :: ts' ->
         let (header, ts'') = visit_LET_HEADER ts' [] []
         in let (body, ts''') = top ts''
            in (match ts''' with
                | [] ->
                   raise (Imparsable "incomplete let expression")
                | RP :: ts'''' ->
                   (Let (header, body), ts'''')
                | _ ->
                   raise (Imparsable "overflowing let abstraction"))
      | _ ->
         raise (Imparsable "illegal header in let expression")
    and visit_LET_HEADER ts hs ss =
      let () = if !silent_flag
               then ()
               else Printf.printf "visit_LET_HEADER %s\n" (unparse_lexemes ts) in
      match ts with
      | LP :: ATOM s :: ts' ->
         if is_a_keyword s
         then raise (Imparsable (Printf.sprintf "keyword %s as formal in let expression" s))
         else if list_member s ss
         then raise (Imparsable "indistinct formals in let expression")
         else let (d, ts'') = top ts'
              in (match ts'' with
                  | RP :: ts''' ->
                     (match ts''' with
                      | RP :: ts'''' ->
                         (List.rev ((s, d) :: hs), ts'''')
                      | _ ->
                         visit_LET_HEADER ts''' ((s, d) :: hs) (s :: ss))
                  | _ ->
                     raise (Imparsable "incorrect header in let expression"))
      | RP :: ts' ->
         (List.rev hs, ts')
      | _ ->
         raise (Imparsable "incorrect header in let expression")
    and visit_LETREC ts =
      let () = if !silent_flag
               then ()
               else Printf.printf "visit_LETREC %s\n" (unparse_lexemes ts) in
      match ts with
      | [] ->
         raise (Imparsable "incomplete letrec expression")
      | LP :: ts' ->
         let (header, ts'') = visit_LETREC_HEADER ts' [] []
         in let (body, ts''') = top ts''
            in (match ts''' with
                | [] ->
                   raise (Imparsable "incomplete letrec expression")
                | RP :: ts'''' ->
                   (Letrec (header, body), ts'''')
                | _ ->
                   raise (Imparsable "overflowing letrec abstraction"))
      | _ ->
         raise (Imparsable "illegal header in letrec expression")
    and visit_LETREC_HEADER ts hs ss =
      let () = if !silent_flag
               then ()
               else Printf.printf "visit_LETREC_HEADER %s\n" (unparse_lexemes ts) in
      match ts with
      | LP :: ATOM s :: ts' ->
         if is_a_keyword s
         then raise (Imparsable (Printf.sprintf "keyword %s as formal in letrec expression" s))
         else if list_member s ss
         then raise (Imparsable "indistinct formals in letrec expression")
         else let (d, ts'') = visit_LAMBDA_ABSTRACTION ts'
              in (match ts'' with
                  | RP :: ts''' ->
                     (match ts''' with
                      | RP :: ts'''' ->
                         (List.rev ((s, d) :: hs), ts'''')
                      | _ ->
                         visit_LETREC_HEADER ts''' ((s, d) :: hs) (s :: ss))
                  | _ ->
                     raise (Imparsable "incorrect header in letrec expression"))
      | RP :: ts' ->
         (List.rev hs, ts')
      | _ ->
         raise (Imparsable "incorrect header in letrec expression")
    and visit_LAMBDA_ABSTRACTION ts =
      let () = if !silent_flag
               then ()
               else Printf.printf "visit_LAMBDA_ABSTRACTION %s\n" (unparse_lexemes ts) in
      match ts with
      | [] ->
         raise (Imparsable "incomplete header in letrec expression")
      | LP :: ATOM "lambda" :: ts' ->
         let (formals, ts'') = visit_FORMALS ts'
         in let () = if !silent_flag
                     then ()
                     else Printf.printf "visit_LAMBDA_ABSTRACTION -- got formals %s\n" (unparse_lexemes ts'') in
            let (body, ts''') = top ts''
            in let () = if !silent_flag
                        then ()
                        else Printf.printf "visit_LAMBDA_ABSTRACTION -- got body %s\n" (unparse_lexemes ts''') in
               (match ts''' with
                | [] ->
                   raise (Imparsable "incomplete lambda-abstraction in letrec header")
                | RP :: ts'''' ->
                   ((formals, body), ts'''')
                | _ ->
                   raise (Imparsable "overflowing lambda-abstraction"))
      | _ ->
         raise (Imparsable "improper header in letrec expression");;

    let parse_expression ts =
      match top ts with
      | (e, []) ->
         Expression e
      | (e, ts') ->
         let () = Printf.printf "extra tokens on top: %s\n" (unparse_lexemes ts')
         in raise (Imparsable "extra tokens");;

    let parse_definition ts =
      match ts with
      | ATOM s :: ts' ->
         if is_a_keyword s
         then raise (Imparsable "keyword in a toplevel definition")
         else (match top ts' with
               | (e, RP :: nil) ->
                  Definition (s, e)
               | _ ->
                  raise (Imparsable "ill-ending toplevel definition"))
      | _ ->
         raise (Imparsable "improper toplevel definition");;

    let parse_top ts =
      match ts with
      | LP :: ATOM "define" :: ts' ->
         parse_definition ts'
      | _ ->
         parse_expression ts;;
  end;;

(* ********** *)

module Parse_Scheme_toplevel_form =
  struct
    let declared_in_letrecp s r =
      let rec loop r =
        match r with
        | [] ->
           None
        | (s', optional_offset) :: r' ->
           if s' = s
           then optional_offset
           else loop r'
      in loop r;;

    let remap_formals formals =
      match formals with
      | Olivier.Single_formal s ->
         Wayne.Args s
      | Olivier.Proper_formals ss ->
         Wayne.Args_list ss
      | Olivier.Improper_formals (ss, s) ->
         Wayne.Args_improper_list (ss, s);;

    let extend_env_formals formals r =
      match formals with
      | Olivier.Single_formal s ->
         ((s, None) :: r)
      | Olivier.Proper_formals ss ->
         List.fold_right (fun x r -> ((x, None) :: r))
                         ss
                         r
      | Olivier.Improper_formals (ss, s) ->
         List.fold_right (fun x r -> ((x, None) :: r))
                         ss
                         ((s, None) :: r);;

    let rec remap_expression e r =
      match e with
      | Olivier.True ->
         Wayne.Bool true
      | Olivier.False ->
         Wayne.Bool false
      | Olivier.Int n ->
         Wayne.Integer n
      | Olivier.Char c ->
         Wayne.Char c
      | Olivier.String s ->
         Wayne.Str s
      | Olivier.Name s ->
         (match declared_in_letrecp s r with
          | None ->
             Wayne.Var s
          | Some offset ->
             Wayne.Var_rec (s, offset))
      | Olivier.Quote e ->
         Wayne.Quote (remap_expression e r)
      | Olivier.If (test, consequent, alternative) ->
         Wayne.If (remap_expression test r,
                   remap_expression consequent r,
                   remap_expression alternative r)
      | Olivier.Lambda (formals, body) ->
         Wayne.Lambda_abstraction (remap_formals formals, remap_expression body (extend_env_formals formals r))
      | Olivier.Let (bindings, body) ->
         let names = List.map (fun (s, _) -> s) bindings
         in Wayne.Let (List.map (fun (s, e) ->
                                  (s, remap_expression e r))
                                bindings,
                       remap_expression body (List.fold_right (fun x r -> ((x, None) :: r))
                                                              names
                                                              r))
      | Olivier.Letrec (bindings, body) ->
         let names = List.map (fun (s, _) -> s) bindings
         in let r' = List.fold_right (fun x c i -> ((x, Some i) :: c (i + 1))) names (fun i -> r) 0
            in Wayne.Let_rec (List.map (fun (s, (formals, body)) ->
                                         (s, (remap_formals formals, remap_expression body (extend_env_formals formals r'))))
                                       bindings,
                              remap_expression body r')
      | Olivier.Apply (e0, es) ->
         Wayne.Apply (remap_expression e0 r,
                      List.map (fun e -> remap_expression e r) es);;

    let remap_toplevel_form tf =
      match tf with
      | Olivier.Definition (s, e) ->
         Wayne.Define (s, remap_expression e [])
      | Olivier.Expression e ->
         Wayne.Exp (remap_expression e []);;

    let parse ts =
      remap_toplevel_form (Olivier.parse_top ts);;        
  end;;

(* ********** *)

(* end of parsing.ml *)

let end_of_file = "parsing.ml";;
