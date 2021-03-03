(* parsing.ml *)
(* Olivier Danvy <danvy@yale-nus.edu.sg> *)
(* Singapore, Sun 28 Feb 2021 & Mon 02 Mar 2021 *)

(* ********** *)
open Lexing
open Syntax

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
      | DOT :: _ ->
         raise (Imparsable "dot out of place")
      | QUOTE :: ts' ->
         let (e, ts'') = top ts'
         in (Quote e, ts'')
      | RP :: _ ->
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
      | (_, ts') ->
         let () = Printf.printf "extra tokens on top: %s\n" (unparse_lexemes ts')
         in raise (Imparsable "extra tokens");;

    let parse_definition ts =
      match ts with
      | ATOM s :: ts' ->
         if is_a_keyword s
         then raise (Imparsable "keyword in a toplevel definition")
         else (match top ts' with
               | (e, RP :: []) ->
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
         Args s
      | Olivier.Proper_formals ss ->
         Args_list ss
      | Olivier.Improper_formals (ss, s) ->
         Args_improper_list (ss, s);;

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
         Bool true
      | Olivier.False ->
         Bool false
      | Olivier.Int n ->
         Integer n
      | Olivier.Char c ->
         Char c
      | Olivier.String s ->
         Str s
      | Olivier.Name s ->
         (match declared_in_letrecp s r with
          | None ->
             Var s
          | Some offset ->
             Var_rec (s, offset))
      | Olivier.Quote e ->
         Quote (remap_expression e r)
      | Olivier.If (test, consequent, alternative) ->
         If (remap_expression test r,
                   remap_expression consequent r,
                   remap_expression alternative r)
      | Olivier.Lambda (formals, body) ->
         Lambda_abstraction (remap_formals formals, remap_expression body (extend_env_formals formals r))
      | Olivier.Let (bindings, body) ->
         let names = List.map (fun (s, _) -> s) bindings
         in Let (List.map (fun (s, e) ->
                                  (s, remap_expression e r))
                                bindings,
                       remap_expression body (List.fold_right (fun x r -> ((x, None) :: r))
                                                              names
                                                              r))
      | Olivier.Letrec (bindings, body) ->
         let names = List.map (fun (s, _) -> s) bindings
         in let r' = List.fold_right (fun x c i -> ((x, Some i) :: c (i + 1))) names (fun _ -> r) 0
            in Let_rec (List.map (fun (s, (formals, body)) ->
                                         (s, (remap_formals formals, remap_expression body (extend_env_formals formals r'))))
                                       bindings,
                              remap_expression body r')
      | Olivier.Apply (e0, es) ->
         Apply (remap_expression e0 r,
                      List.map (fun e -> remap_expression e r) es);;

    let remap_toplevel_form tf =
      match tf with
      | Olivier.Definition (s, e) ->
         Define (s, remap_expression e [])
      | Olivier.Expression e ->
         Exp (remap_expression e []);;

    let parse ts =
      remap_toplevel_form (Olivier.parse_top ts);;        
  end;;

(* ********** *)

(* end of parsing.ml *)

let end_of_file = "parsing.ml";;
