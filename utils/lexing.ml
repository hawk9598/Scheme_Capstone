(* lexing.ml *)
(* Olivier Danvy <danvy@yale-nus.edu.sg> *)
(* Singapore, Sun 28 Feb 2021 *)

(* ********** *)

type lexeme = LP | RP | DOT | QUOTE | TRUE | FALSE | CHAR of char | STRING of string | INT of int | ATOM of string;;

let unparse_lexemes xs =
  String.concat "" (List.map (fun lexeme ->
                               match lexeme with
                               | LP ->
                                  "("
                               | RP ->
                                  ")"
                               | DOT ->
                                  " . "
                               | QUOTE ->
                                  " '"
                               | TRUE ->
                                  "#t"
                               | FALSE ->
                                  "#f"
                               | CHAR c ->
                                  "#\\" ^ String.make 1 c
                               | STRING s ->
                                  "\"" ^ s ^ "\""
                               | INT n ->
                                  string_of_int n
                               | ATOM s ->
                                  s)
                             xs);;

exception Incorrect_string;;

(*
let test_lex candidate =
  let b0 = (candidate "1 22 333 ~-4444" = [INT 1; INT 22; INT 333; INT (-4444)])
  and b1 = (candidate "+ -/predsucc" = [PLUS; MINUS; QUOTIENT; PRED; SUCC])
  and b2 = (candidate ")(((" = [RP; LP; LP; LP])
  and b3 = (candidate "((1 + 2) * ~-3)" = [LP; LP; INT 1; PLUS; INT 2; RP; TIMES; INT (-3); RP])
  in b0 && b1 && b2 && b3;;
*)

let lex cs =
  let length_cs = String.length cs
  in let rec visit i =
       if i = length_cs
       then []
       else match String.get cs i with
            | ' ' ->
               visit (i + 1)
            | '(' ->
               LP :: visit (i + 1)
            | ')' ->
               RP :: visit (i + 1)
            | '.' ->
               DOT :: visit (i + 1)
            | '\'' ->
               QUOTE :: visit (i + 1)
            | '#' ->
               if i + 1 < length_cs
               then match String.get cs (i + 1) with
                    | 't' ->
                       if i + 2 = length_cs
                       then TRUE :: []
                       else (match String.get cs (i + 2) with
                             | '(' | ')' | ' ' ->
                                TRUE :: visit (i + 2)
                             | _ ->
                                (Printf.printf "Incorrect string \"%s\" at index %d\n" cs i;
                                   raise Incorrect_string))
                    | 'f' ->
                       if i + 2 = length_cs
                       then FALSE :: []
                       else (match String.get cs (i + 2) with
                             | '(' | ')' | ' ' ->
                                FALSE :: visit (i + 2)
                             | _ ->
                                (Printf.printf "Incorrect string \"%s\" at index %d\n" cs i;
                                 raise Incorrect_string))
                    | '\\' ->
                       if i + 2 < length_cs
                       then CHAR (String.get cs (i + 2)) :: visit (i + 3)
                       else (Printf.printf "Incorrect string \"%s\" at index %d\n" cs i;
                             raise Incorrect_string)
                    | _ ->
                       (Printf.printf "Incorrect string \"%s\" at index %d\n" cs i;
                        raise Incorrect_string)
               else (Printf.printf "Incorrect string \"%s\" at index %d\n" cs i;
                        raise Incorrect_string)
            | '"' ->
               let rec scan j =
                 if j = length_cs
                 then (Printf.printf "Incorrect string \"%s\" at index %d\n" cs i;
                       raise Incorrect_string)
                 else match String.get cs j with
                      | '"' ->
                         STRING (String.sub cs i (j - i + 1)) :: visit (j + 1)
                      | '\\' ->
                         if j + 1 < length_cs
                         then scan (j + 2)
                         else (Printf.printf "Incorrect string \"%s\" at index %d\n" cs i;
                               raise Incorrect_string)
                      | _ ->
                         scan (j + 1)
               in scan (i + 1)
                
            | c ->
               let rec compute i n =
                 if i = length_cs
                 then INT n :: []
                 else let c = String.get cs i
                      in if int_of_char '0' <= int_of_char c && int_of_char c <= int_of_char '9'
                         then compute (i + 1) (10 * n + (int_of_char c - int_of_char '0'))
                         else (match c with
                               | '(' | ')' | ' ' ->
                                  INT n :: visit i
                               | _ ->
                                  (Printf.printf "Incorrect string \"%s\" at index %d\n" cs i;
                                   raise Incorrect_string))
               and scan j =
                 if j = length_cs
                 then ATOM (String.sub cs i (j - i)) :: []
                 else match String.get cs j with
                      | '(' | ')' | ' ' ->
                         ATOM (String.sub cs i (j - i)) :: visit j
                      | _ ->
                         scan (j + 1)
               in if int_of_char '0' <= int_of_char c && int_of_char c <= int_of_char '9'
                  then compute (i + 1) (int_of_char c - int_of_char '0')
                  else
                    match c with
                    (* Must consider the special case where - is used to get negative of a number *)
                    |'-' -> if i + 1 < length_cs
                            then let rec compute_neg i n =
                                   if i = length_cs
                                   then INT (- n) :: []
                                   else let c = String.get cs i
                                        in if int_of_char '0' <= int_of_char c && int_of_char c <= int_of_char '9'
                                           then compute_neg (i + 1) (10 * n + (int_of_char c - int_of_char '0'))
                                           else INT (- n) :: visit i
                                 in let c' = String.get cs (i + 1)
                                    in if int_of_char '0' <= int_of_char c' && int_of_char c' <= int_of_char '9'
                                       then compute_neg (i + 2) (int_of_char c' - int_of_char '0')
                                       else
                                         match c' with
                                         (* this is probably the case where - is a primitive due to the next character being a space *)
                                         |' ' -> scan (i + 1)
                                         |_ -> (Printf.printf "Incorrect string \"%s\" at index %d\n" cs i;
                                             raise Incorrect_string)
                            else (Printf.printf "Incorrect string \"%s\" at index %d\n" cs i;
                                  raise Incorrect_string)
                    |_ -> scan (i + 1)
  in visit 0;;

(*
let () = assert (test_lex lex);;
*)

(* ********** *)

(* end of lexing.ml *)

let end_of_file = "lexing.ml";;
