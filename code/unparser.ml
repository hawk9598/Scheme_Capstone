open Ast

let show_list show_yourself vs =
  match vs with
  | [] ->
     "[]"
  | v :: vs' ->
     let rec show_list_aux v vs' =
       match vs' with
       | [] ->
          show_yourself v
       | v' :: vs'' ->
          (show_yourself v) ^ "; " ^ (show_list_aux v' vs'')
     in "[" ^ show_list_aux v vs' ^ "]"

let test_show_string candidate =
  let b0 = (candidate "string" = "\"string\"")
  and b1 = (candidate "Hello World" = "\"Hello World\"")
  and b2 = (candidate "Hawk9598" = "\"Hawk9598\"")
  and b3 = (candidate "[1;2;3;4;5]" = "\"[1;2;3;4;5]\"")
  and b4 = (candidate "(5, 3)" = "\"(5, 3)\"")
  and b5 = (candidate "test_show_string" = "\"test_show_string\"")
  in b0 && b1 && b2 && b3 && b4 && b5
   
let show_string s =
  (* show_string : string -> string *)
  "\"" ^ s ^ "\"";;

assert (test_show_string show_string)

let test_show_int candidate =
  let b0 = (candidate 5 = "5")
  and b1 = (candidate 100 = "100")
  and b2 = (candidate (-5) = "~-5")
  and b3 = (candidate (-100) = "~-100")
  and b4 = (candidate 0 = "0")
  in b0 && b1 && b2 && b3 && b4;;

let show_int n =
  if n < 0
  then "~" ^ string_of_int n
  else string_of_int n;;

assert (test_show_int show_int);;

let test_show_bool candidate =
  let b0 = (candidate true = "true")
  and b1 = (candidate false = "false")
  in b0 && b1

let show_bool b =
  if b
  then "true"
  else "false";;

assert (test_show_bool show_bool)

let test_show_char candidate =
  let b0 = (candidate 'c' = "'c'")
  and b1 = (candidate 'd' = "'d'")
  and b2 = (candidate 'a' = "'a'")
  and b3 = (candidate 'z' = "'z'")
  and b4 = (candidate '5' = "'5'")
  and b5 = (candidate '9' = "'9'")
  in b0 && b1 && b2 && b3 && b4 && b5;;

let show_char c =
  "'" ^ String.make 1 c ^ "'";;

assert (test_show_char show_char);;

let test_show_exp_val (candidate : exp_val -> string) =
  let b0 = (candidate (Int 5) = "5")
  and b1 = (candidate (Int (-5)) = "~-5")
  and b2 = (candidate (Boolean true) = "true")
  and b3 = (candidate (Boolean false) = "false")
  and b4 = (candidate (String "string") = "\"string\"")
  and b5 = (candidate (String "interpreter") = "\"interpreter\"")
  and b6 = (candidate (Character 'c') = "'c'")
  and b7 = (candidate (Character 'n') = "'n'")
  and b8 = (candidate (Pair (Int 5, Int 6)) = "(5 , 6)")
  and b9 = (candidate (Pair (Int (-5), Int (-99))) = "(~-5 , ~-99)")
  and b10 = (candidate (Pair (Boolean true, Boolean false)) = "(true , false)")
  and b11 = (candidate (Pair (String "self", String "interpreter")) = "(\"self\" , \"interpreter\")")
  and b12 = (candidate (Pair (Character 'n', Character 'a')) = "('n' , 'a')")
  and b13 = (candidate (Pair (Pair (Int 6, Int 9), Pair(Boolean true, Boolean false))) = "((6 , 9) , (true , false))")
  and b14 = (candidate (Pair (Character 'c', Pair (String "hello", String "world"))) = "('c' , (\"hello\" , \"world\"))")
  in b0 && b1 && b2 && b3 && b4 && b5 && b6 && b7 && b8 && b9 && b10 && b11 && b12 && b13 && b14;;

(*  let test_show_exp_val_fail candidate =
    let b0 = (try ignore (candidate (Closure (fun vs -> begin
                                                  match vs with
                                                  |[] ->
                                                    Boolean false
                                                  |v :: vs'->
                                                    v
                                                end)));
                  failwith "bad show_exp_val" with Error "Unable to show functions."-> ())
    and b1 = (try ignore (candidate (Primitive (fun v -> v)));
                  failwith "bad show_exp_val" with Error "Unable to show functions."-> ())
    in b0 ; b1;; *)


let rec show_exp_val (v : exp_val): string  =
  (* show_exp_val : exp_val -> string *)
  begin
    match v with
    |Int i ->
      show_int i     
    |Boolean b ->
      show_bool b
    |String s ->
      show_string s
    |Character c ->
      show_char c
    |Pair (v1, v2) ->
      "(" ^ (show_exp_val v1) ^ " , " ^ (show_exp_val v2) ^ ")"
    |Primitive _->
      "Primitive function."
    |Closure _ ->
      "Closure function."
    |Null ->
      "[]"
  end;;

assert (test_show_exp_val show_exp_val);;
(* (test_show_exp_val_fail show_exp_val);; *) 
