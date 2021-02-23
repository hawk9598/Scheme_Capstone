open Unparser_cps
open Ast_cps

exception Error of string

(* Defining the apply procedure *)

(* Maps a scheme value into an OCaml list of scheme values. *)
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
  
(* Defining internal functions for pairs and lists *)
                 
let internal_is_pair =
  (fun vs ->
    begin
      match vs with
      |v :: [] ->
        begin
          match v with
          |Pair (_, _) -> 
            Boolean true
          |_ ->
            Boolean false
        end
      |_ ->
        raise (Error
                 (Printf.sprintf
                    "Incorrect argument count in call (pair? %s)"
                    (show_list show_exp_val vs)))
    end)

let internal_cons =
  (fun vs ->
    begin
      match vs with
      |v1 :: v2 :: [] ->
        Pair (v1, v2)
      |_ ->
        raise (Error
                 (Printf.sprintf
                    "Incorrect argument count in call (cons %s)"
                    (show_list show_exp_val vs)))
    end)


(*Pair (ref v1, ref v2) *)
(* let internal_set_car vs =
  begin             
    match vs with
    |v1 :: v2 :: [] ->
      begin
        match v1 with
        |Pair (r1, _) ->
          r1:= v2;
          v1
        |_ ->
          raise (Error
                   (Printf.sprintf
                      "Error in set-car!: %s is not a pair."
                      (show_exp_val v1)))
      end
     
     
    |_ ->
      raise (Error
               (Printf.sprintf
                  "Incorrect argument count in call %s"
                  (show_list show_exp_val vs)))
  end

let internal_set_cdr vs =
  begin
    match vs with
    |v1 :: v2 :: [] ->
      begin
        match v1 with
        |Pair (_, r2) ->
          r2:= v2;
          v1
        |_ ->
          raise (Error
                   (Printf.sprintf
                      "Error in set-cdr!: %s is not a pair."
                      (show_exp_val v1)))
      end
     
     
    |_ ->
      raise (Error
               (Printf.sprintf
                  "Incorrect argument count in call %s"
                  (show_list show_exp_val vs)))
  end
 *)
(* !v1 *)
  

let internal_car =
  (fun vs ->
    begin
      match vs with
      |v :: [] ->
        begin
          match v with
          |Pair (v1, _) ->
            v1 
          |_ ->
            raise (Error
                     (Printf.sprintf
                        "Error in car: %s is not a pair."
                        (show_exp_val v)))
        end
       
       
      |_ ->
        raise (Error
                 (Printf.sprintf
                    "Incorrect argument count in call (car %s)"
                    (show_list show_exp_val vs)))
    end)
  
let internal_cdr =
  (fun vs ->
    begin
      match vs with
      |v :: [] ->
        begin
          match v with
          |Pair (_, v2) ->
            v2 (* !v2 *)
          |_ ->
            raise (Error
                     (Printf.sprintf
                        "Error in cdr: %s is not a pair."
                        (show_exp_val v)))
        end

      |_ ->
        raise (Error
                 (Printf.sprintf
                    "Incorrect argument count in call (cdr %s)"
                    (show_list show_exp_val vs)))
    end)

(* Defining internal functions for numerical operations *)

let internal_is_int =
  (fun vs ->
    begin
      match vs with
      |v :: [] ->
        begin
          match v with
          |Int _ ->
            Boolean true
          |_ ->
            Boolean false
        end
      |_ ->
        raise (Error
                 (Printf.sprintf
                    "Incorrect argument count in call (integer? %s)"
                    (show_list show_exp_val vs)))
    end)

let internal_is_zero =
  (fun vs ->
    begin
      match vs with
      |v :: [] ->
        begin
          match v with
          |Int i ->
            if i = 0
            then Boolean true
            else
              Boolean false
          |_ ->
            raise (Error
                     (Printf.sprintf
                        "Error in zero?: %s is not a number."
                        (show_exp_val v)))
        end
      |_ ->
        raise (Error
                 (Printf.sprintf
                    "Incorrect argument count in call (zero? %s)"
                    (show_list show_exp_val vs)))
    end)

let internal_is_positive =
  (fun vs ->
    begin
      match vs with
      |v :: [] ->
        begin
          match v with
          |Int i ->
            if i > 0
            then Boolean true
            else
              Boolean false
          |_ ->
            raise (Error
                     (Printf.sprintf
                        "Error in positive?: %s is not a number."
                        (show_exp_val v)))
        end
      |_ ->
        raise (Error
                 (Printf.sprintf
                    "Incorrect argument count in call (positive? %s)"
                    (show_list show_exp_val vs)))
    end)

let internal_is_negative =
  (fun vs ->
    begin
      match vs with
      |v :: [] ->
        begin
          match v with
          |Int i ->
            if i < 0
            then Boolean true
            else
              Boolean false
          |_ ->
            raise (Error
                     (Printf.sprintf
                        "Error in negative?: %s is not a number."
                        (show_exp_val v)))
        end
      |_ ->
        raise (Error
                 (Printf.sprintf
                    "Incorrect argument count in call (negative? %s)"
                    (show_list show_exp_val vs)))
    end)

let internal_is_even =
  (fun vs ->
    begin
      match vs with
      |v :: [] ->
        begin
          match v with
          |Int i ->
            if (i mod 2) = 0
            then Boolean true
            else
              Boolean false
          |_ ->
            raise (Error
                     (Printf.sprintf
                        "Error in even?: %s is not a number."
                        (show_exp_val v)))
        end
      |_ ->
        raise (Error
                 (Printf.sprintf
                    "Incorrect argument count in call (even? %s)"
                    (show_list show_exp_val vs)))
    end)

let internal_is_odd =
  (fun vs ->
    begin
      match vs with
      |v :: [] ->
        begin
          match v with
          |Int i ->
            if (i mod 2) != 0
            then Boolean true
            else
              Boolean false
          |_ ->
            raise (Error
                     (Printf.sprintf
                        "Error in odd?: %s is not a number."
                        (show_exp_val v)))
        end
      |_ ->
        raise (Error
                 (Printf.sprintf
                    "Incorrect argument count in call (odd? %s)"
                    (show_list show_exp_val vs)))
    end)  
  
let internal_add =
  (fun vs ->
    let rec visit_add vs' a =
      begin
        match vs' with
        |[] ->
          Int a
        |v :: vs'' ->
          begin
            match v with
            |Int i ->
              visit_add vs'' (a + i)
            |_ ->
              raise (Error
                       (Printf.sprintf
                          "Error in +: %s is not a number."
                          (show_exp_val v)))
          end
      end
    in visit_add vs 0)

let internal_sub =
  (fun vs ->
    begin
      match vs with
      |[] ->
        raise (Error
                 (Printf.sprintf
                    "Incorrect argument count in call (- %s)"
                    (show_list show_exp_val vs)))
      |v :: [] ->
        begin
          match v with
          |Int i ->
            Int (-i)
          |_ ->
            raise (Error
                     (Printf.sprintf
                        "Error in -: %s is not a number."
                        (show_exp_val v)))
        end
      |v1 :: vs' ->
        begin
          match v1 with
          |Int i1 ->
            (let rec visit_sub vs' a =
               begin
                 match vs' with
                 |[] ->
                   Int a
                 |v :: vs'' ->
                   begin
                     match v with
                     |Int i ->
                       visit_sub vs'' (a - i)
                     |_ ->
                       raise (Error
                                (Printf.sprintf
                                   "Error in -: %s is not a number."
                                   (show_exp_val v)))
                   end
               end
             in visit_sub vs' i1)
          |_ ->
            raise (Error
                     (Printf.sprintf
                        "Error in -: %s is not a number."
                        (show_exp_val v1)))
        end
    end)

let internal_mul =
  (fun vs ->
    let rec visit_mul vs' a =
      match vs' with
      |[] ->
        Int a
      |v :: vs'' ->
        begin
          match v with
          |Int i ->
            visit_mul vs'' (a * i)
          |_ ->
            raise (Error
                     (Printf.sprintf
                        "Error in *: %s is not a number."
                        (show_exp_val v)))
        end
    in visit_mul vs 1)

let internal_div =
  (fun vs ->
    begin
      match vs with
      |[] ->
        raise (Error
                 (Printf.sprintf
                    "Incorrect argument count in call (/ %s)"
                    (show_list show_exp_val vs)))
      |v :: [] ->
        begin
          match v with
          |Int  i ->
            Int (1 / i)
          |_ ->
            raise (Error
                     (Printf.sprintf
                        "Error in /: %s is not a number."
                        (show_exp_val v)))
        end
      |v1 :: vs' ->
        begin
          match v1 with
          |Int i1 ->
            (let rec visit_div vs' a =
               begin
                 match vs' with
                 |[] ->
                   Int a
                 |v :: vs'' ->
                   begin
                     match v with
                     |Int i ->
                       if i = 0
                       then raise (Error
                                     (Printf.sprintf
                                        "Error in /: undefined for 0."))
                       else
                         visit_div vs'' (a / i)
                     |_ ->
                       raise (Error
                                (Printf.sprintf
                                   "Error in /: %s is not a number."
                                   (show_exp_val v)))
                   end
               end
             in visit_div vs' i1)
          |_ ->
            raise (Error
                     (Printf.sprintf
                        "Error in /: %s is not a number."
                        (show_exp_val v1)))
        end
    end)

let internal_quotient =
  (fun vs ->
    begin
      match vs with
      |v1 :: v2 :: [] ->
        begin
          match v1 with
          |Int i1 ->
            begin
              match v2 with
              |Int i2 ->
                if i2 = 0
                then raise (Error
                              (Printf.sprintf
                                 "Error in quotient: undefined for 0."))
                else
                  Int (i1 / i2)
              |_ ->
                raise (Error
                         (Printf.sprintf
                            "Error in quotient: %s is not a number."
                            (show_exp_val v2)))
            end
          |_ ->
            raise (Error
                     (Printf.sprintf
                        "Error in quotient: %s is not a number."
                        (show_exp_val v1)))
        end
      |_ ->
        raise (Error
                 (Printf.sprintf
                    "Incorrect argument count in call (quotient %s)"
                    (show_list show_exp_val vs)))
    end)

let internal_remainder =
  (fun vs ->
    begin
      match vs with
      |v1 :: v2 :: [] ->
        begin
          match v1 with
          |Int i1 ->
            begin
              match v2 with
              |Int i2 ->
                if i2 = 0
                then raise (Error
                              (Printf.sprintf
                                 "Error in remainder: undefined for 0."))
                else
                  Int (i1 mod i2)
              |_ ->
                raise (Error
                         (Printf.sprintf
                            "Error in remainder: %s is not a number."
                            (show_exp_val v2)))
            end
          |_ ->
            raise (Error
                     (Printf.sprintf
                        "Error in remainder: %s is not a number."
                        (show_exp_val v1)))
        end
      |_ ->
        raise (Error
                 (Printf.sprintf
                    "Incorrect argument count in call (remainder %s)"
                    (show_list show_exp_val vs)))
    end)
  
let internal_exponentiation =
  (fun vs ->
    begin
      match vs with
      |v1 :: v2 :: [] ->
        begin
          match v1 with
          |Int i1 ->
            begin
              match v2 with
              |Int i2 ->
                if i2 > 0 then
                  let rec power x n a =
                    if n == 0
                    then Int a
                    else
                      let n' = n - 1 in
                      power x n' (x * a)
                  in power i1 i2 1
                else
                  let rec power x n a =
                    if n == 0
                    then Int (1/a)
                    else
                      let n' = n - 1 in
                      power x n' (x * a)
                  in power i1 (-i2) 1
              |_ ->
                raise (Error
                         (Printf.sprintf
                            "Error in expt: %s is not a number."
                            (show_exp_val v2)))
            end
          |_ ->
            raise (Error
                     (Printf.sprintf
                        "Error in expt: %s is not a number."
                        (show_exp_val v1)))
        end       
      |_ ->
        raise (Error
                 (Printf.sprintf
                    "Incorrect argument count in call (expt %s)"
                    (show_list show_exp_val vs)))
    end)
          

let internal_lt =
  (fun vs ->
    begin
      match vs with
      |[] ->
        raise (Error
                 (Printf.sprintf
                    "Incorrect argument count in call (< %s)"
                    (show_list show_exp_val vs)))
      |v1 :: vs' ->
        begin match v1 with
        |Int i1 ->
          let rec visit_lt vs i' a =
            begin
              match vs with
              |[] -> Boolean a
              |v :: vs' ->
                begin
                  match v with
                  |Int i ->
                    if i > i'
                    then visit_lt vs' i a
                    else
                      if a
                      then visit_lt vs' i false
                      else
                        visit_lt vs' i a
                  |_ ->
                    raise (Error
                             (Printf.sprintf
                                "Error in <: %s is not a number."
                                (show_exp_val v)))
                end
            end
          in visit_lt vs' i1 true
        |_ ->
          raise (Error
                   (Printf.sprintf
                      "Error in <: %s is not a number."
                      (show_exp_val v1)))
        end
    end)
  
let internal_lte =
  (fun vs ->
    begin
      match vs with
      |[] ->
        raise (Error
                 (Printf.sprintf
                    "Incorrect argument count in call (<= %s)"
                    (show_list show_exp_val vs)))
      |v1 :: vs' ->
        begin match v1 with
        |Int i1 ->
          let rec visit_lte vs i' a =
            begin
              match vs with
              |[] -> Boolean a
              |v :: vs' ->
                begin
                  match v with
                  |Int i ->
                    if i >= i'
                    then visit_lte vs' i a
                    else
                      if a
                      then visit_lte vs' i false
                      else
                        visit_lte vs' i a
                  |_ ->
                    raise (Error
                             (Printf.sprintf
                                "Error in <=: %s is not a number."
                                (show_exp_val v)))
                end
            end
          in visit_lte vs' i1 true
        |_ ->
          raise (Error
                   (Printf.sprintf
                      "Error in <=: %s is not a number."
                      (show_exp_val v1)))
        end
    end)               
          
let internal_gt =
  (fun vs ->
    begin
      match vs with
      |[] ->
        raise (Error
                 (Printf.sprintf
                    "Incorrect argument count in call (> %s)"
                    (show_list show_exp_val vs)))
      |v1 :: vs' ->
        begin match v1 with
        |Int i1 ->
          let rec visit_gt vs i' a =
            begin
              match vs with
              |[] -> Boolean a
              |v :: vs' ->
                begin
                  match v with
                  |Int i ->
                    if i < i'
                    then visit_gt vs' i a
                    else
                      if a
                      then visit_gt vs' i false
                      else
                        visit_gt vs' i a
                  |_ ->
                    raise (Error
                             (Printf.sprintf
                                "Error in >: %s is not a number."
                                (show_exp_val v)))
                end
            end
          in visit_gt vs' i1 true
        |_ ->
          raise (Error
                   (Printf.sprintf
                      "Error in >: %s is not a number."
                      (show_exp_val v1)))
        end
    end)

let internal_gte =
  (fun vs ->
    begin
      match vs with
      |[] ->
        raise (Error
                 (Printf.sprintf
                    "Incorrect argument count in call (>= %s)"
                    (show_list show_exp_val vs)))
      |v1 :: vs' ->
        begin match v1 with
        |Int i1 ->
          let rec visit_gte vs i' a =
            begin
              match vs with
              |[] -> Boolean a
              |v :: vs' ->
                begin
                  match v with
                  |Int i ->
                    if i <= i'
                    then visit_gte vs' i a
                    else
                      if a
                      then visit_gte vs' i false
                      else
                        visit_gte vs' i a
                  |_ ->
                    raise (Error
                             (Printf.sprintf
                                "Error in >=: %s is not a number."
                                (show_exp_val v)))
                end
            end
          in visit_gte vs' i1 true
        |_ ->
          raise (Error
                   (Printf.sprintf
                      "Error in >=: %s is not a number."
                      (show_exp_val v1)))
        end
    end)
  
let internal_equal =
  (fun vs ->
    begin
      match vs with
      |[] ->
        raise (Error
                 (Printf.sprintf
                    "Incorrect argument count in call (= %s)"
                    (show_list show_exp_val vs)))
      |v1 :: vs' ->
        begin
          match v1 with
          |Int i1 ->
            let rec visit_equal vs i' a =
              begin
                match vs with
                |[] ->
                  Boolean a
                |v :: vs' ->
                  begin match v with
                  |Int i ->
                    if i = i'
                    then visit_equal vs' i a
                    else
                      if a
                      then visit_equal vs' i false
                      else
                        visit_equal vs' i a
                  |_ ->
                    raise (Error
                             (Printf.sprintf
                                "Error in =: %s is not a number."
                                (show_exp_val v)))
                  end
              end
            in visit_equal vs' i1 true

          |_ ->
            raise (Error
                     (Printf.sprintf
                        "Error in =: %s is not a number."
                        (show_exp_val v1)))
        end
    end)

(* Defining operations on Characters *)
  
let internal_is_char =
  (fun vs ->
    begin
      match vs with
      |v :: [] ->
        begin
          match v with
          |Character _ ->
            Boolean true
          |_ ->
            Boolean false
        end
      |_ ->
        raise (Error
                 (Printf.sprintf
                    "Incorrect argument count in call (char? %s)"
                    (show_list show_exp_val vs)))
    end)

let internal_char_equal =
  (fun vs ->
    begin
      match vs with
      |[] ->
        raise (Error
                 (Printf.sprintf
                    "Incorrect argument count in call (char=? %s)"
                    (show_list show_exp_val vs)))
      |v1 :: vs' ->
        begin
          match v1 with
          |Character c1 ->
            let rec visit_char_equal vs c' a =
              begin
                match vs with
                |[] ->
                  Boolean a
                |v :: vs' ->
                  begin
                    match v with
                    |Character c ->
                      if c = c'
                      then visit_char_equal vs' c a
                      else
                        if a
                        then visit_char_equal vs' c false
                        else
                          visit_char_equal vs' c a
                    |_ ->
                      raise (Error
                               (Printf.sprintf
                                  "Error in char=?: %s is not a character."
                                  (show_exp_val v)))
                  end
              end
            in visit_char_equal vs' c1 true
          |_ ->
            raise (Error
                     (Printf.sprintf
                        "Error in char=?: %s is not a character."
                        (show_exp_val v1)))
        end
    end)

let internal_char_gt =
  (fun vs ->
    begin
      match vs with
      |[] ->
        raise (Error
                 (Printf.sprintf
                    "Incorrect argument count in call (char>? %s)"
                    (show_list show_exp_val vs)))
      |v1 :: vs' ->
        begin
          match v1 with
          |Character c1 ->
            let rec visit_char_gt vs c' a =
              begin
                match vs with
                |[] ->
                  Boolean a
                |v :: vs' ->
                  begin
                    match v with
                    |Character c ->
                      if c < c'
                      then visit_char_gt vs' c a
                      else
                        if a
                        then visit_char_gt vs' c false
                        else
                          visit_char_gt vs' c a
                    |_ ->
                      raise (Error
                               (Printf.sprintf
                                  "Error in char>?: %s is not a character."
                                  (show_exp_val v)))
                  end
              end
            in visit_char_gt vs' c1 true
          |_ ->
            raise (Error
                     (Printf.sprintf
                        "Error in char>?: %s is not a character."
                        (show_exp_val v1)))
        end
    end)
  
let internal_char_ge =
  (fun vs ->
    begin
      match vs with
      |[] ->
        raise (Error
                 (Printf.sprintf
                    "Incorrect argument count in call (char>=? %s)"
                    (show_list show_exp_val vs)))
      |v1 :: vs' ->
        begin
          match v1 with
          |Character c1 ->
            let rec visit_char_ge vs c' a =
              begin
                match vs with
                |[] ->
                  Boolean a
                |v :: vs' ->
                  begin
                    match v with
                    |Character c ->
                      if c <= c'
                      then visit_char_ge vs' c a
                      else
                        if a
                        then visit_char_ge vs' c false
                        else
                          visit_char_ge vs' c a
                    |_ ->
                      raise (Error
                               (Printf.sprintf
                                  "Error in char>=?: %s is not a character."
                                  (show_exp_val v)))
                  end
              end
            in visit_char_ge vs' c1 true
          |_ ->
            raise (Error
                     (Printf.sprintf
                        "Error in char>=?: %s is not a character."
                        (show_exp_val v1)))
        end
    end)


let internal_char_lt =
  (fun vs ->
    begin
      match vs with
      |[] ->
        raise (Error
                 (Printf.sprintf
                    "Incorrect argument count in call (char<? %s)"
                    (show_list show_exp_val vs)))
      |v1 :: vs' ->
        begin
          match v1 with
          |Character c1 ->
            let rec visit_char_lt vs c' a =
              begin
                match vs with
                |[] ->
                  Boolean a
                |v :: vs' ->
                  begin
                    match v with
                    |Character c ->
                      if c > c'
                      then visit_char_lt vs' c a
                      else
                        if a
                        then visit_char_lt vs' c false
                        else
                          visit_char_lt vs' c a
                    |_ ->
                      raise (Error
                               (Printf.sprintf
                                  "Error in char<?: %s is not a character."
                                  (show_exp_val v)))
                  end
              end
            in visit_char_lt vs' c1 true
          |_ ->
            raise (Error
                     (Printf.sprintf
                        "Error in char<?: %s is not a character."
                        (show_exp_val v1)))
        end
    end)


let internal_char_le =
  (fun vs ->
    begin
      match vs with
      |[] ->
        raise (Error
                 (Printf.sprintf
                    "Incorrect argument count in call (char<=? %s)"
                    (show_list show_exp_val vs)))
      |v1 :: vs' ->
        begin
          match v1 with
          |Character c1 ->
            let rec visit_char_le vs c' a =
              begin
                match vs with
                |[] ->
                  Boolean a
                |v :: vs' ->
                  begin
                    match v with
                    |Character c ->
                      if c >= c'
                      then visit_char_le vs' c a
                      else
                        if a
                        then visit_char_le vs' c false
                        else
                          visit_char_le vs' c a
                    |_ ->
                      raise (Error
                               (Printf.sprintf
                                  "Error in char<=?: %s is not a character."
                                  (show_exp_val v)))
                  end
              end
            in visit_char_le vs' c1 true
          |_ ->
            raise (Error
                     (Printf.sprintf
                        "Error in char<=?: %s is not a character."
                        (show_exp_val v1)))
        end
    end)

let internal_char_numeric =
  (fun vs ->
    begin
      match vs with
      |v :: [] ->
        begin
          match v with
          |Character c ->
            begin
              match c with
              |'0' |'1' |'2' |'3' |'4' |'5' |'6' |'7' |'8' |'9' ->
                Boolean true
              |_ ->
                Boolean false
            end
          |_ ->
            raise
              (Error
                 (Printf.sprintf
                    "Error in char-numeric?: %s is not a character."
                    (show_exp_val v)))
        end
      |_ ->
        raise (Error
                 (Printf.sprintf
                    "Incorrect argument count in call (char-numeric? %s)"
                    (show_list show_exp_val vs)))
    end)

let internal_char_alphabetic = 
  (fun vs ->
    begin
      match vs with
      |v :: [] ->
        begin
          match v with
          |Character c ->
            begin
              match c with
              |'a' |'b' |'c' |'d' |'e' |'f' |'g' |'h' |'i' |'j' |'k' |'l' |'m'
               |'n' |'o' |'p' |'q' |'r' |'s' |'t' |'u' |'v' |'w' |'x' |'y' |'z'
               |'A' |'B' |'C' |'D' |'E' |'F' |'G'| 'H'| 'I' |'J' |'K' |'L' |'M'
               |'N' |'O' |'P' |'Q' |'R' |'S' |'T' |'U' |'V' |'W' |'X' |'Y' |'Z'
               ->
                Boolean true
              |_ -> Boolean false
            end
          |_ ->
            raise
              (Error
                 (Printf.sprintf
                    "Error in char-alphabetic?: %s is not a character."
                    (show_exp_val v)))
        end
      |_ ->
        raise (Error
                 (Printf.sprintf
                    "Incorrect argument count in call (char-alphabetic? %s)"
                    (show_list show_exp_val vs)))
    end)

(* Defining operations on Strings *)
  
let internal_is_str =
  (fun vs ->
    begin
      match vs with
      |v :: [] ->
        begin
          match v with
          |String _ ->
            Boolean true
          |_ ->
            Boolean false
        end
      |_ ->
        raise (Error
                 (Printf.sprintf
                    "Incorrect argument count in call (string? %s)"
                    (show_list show_exp_val vs)))
    end)

  
let internal_str_equal =
  (fun vs ->
    begin
      match vs with
      |[] ->
        raise (Error
                 (Printf.sprintf
                    "Incorrect argument count in call (string=? %s)"
                    (show_list show_exp_val vs)))
      |v1 :: vs' ->
        begin
          match v1 with
          |String s1 ->
            let rec visit_str_equal vs s' a =
              begin
                match vs with
                |[] ->
                  Boolean a
                |v :: vs' ->
                  begin
                    match v with
                    |String s ->
                      if s = s'
                      then visit_str_equal vs' s a
                      else
                        if a
                        then visit_str_equal vs' s false
                        else
                          visit_str_equal vs' s a
                    |_ ->
                      raise (Error
                               (Printf.sprintf
                                  "Error in string=?: %s is not a string."
                                  (show_exp_val v)))
                  end
              end
            in visit_str_equal vs' s1 true
          |_ ->
            raise (Error
                     (Printf.sprintf
                        "Error in string=?: %s is not a string."
                        (show_exp_val v1)))
        end
    end)

let internal_str_gt =
  (fun vs ->
    begin
      match vs with
      |[] ->
        raise (Error
                 (Printf.sprintf
                    "Incorrect argument count in call (string>? %s)"
                    (show_list show_exp_val vs)))
      |v1 :: vs' ->
        begin
          match v1 with
          |String s1 ->
            let rec visit_str_gt vs s' a =
              begin
                match vs with
                |[] ->
                  Boolean a
                |v :: vs' ->
                  begin
                    match v with
                    |String s ->
                      if s < s'
                      then visit_str_gt vs' s a
                      else
                        if a
                        then visit_str_gt vs' s false
                        else
                          visit_str_gt vs' s a
                    |_ ->
                      raise (Error
                               (Printf.sprintf
                                  "Error in string>?: %s is not a string."
                                  (show_exp_val v)))
                  end
              end
            in visit_str_gt vs' s1 true
          |_ ->
            raise (Error
                     (Printf.sprintf
                        "Error in string>?: %s is not a string."
                        (show_exp_val v1)))
        end
    end)
  
let internal_str_ge =
  (fun vs ->
    begin
      match vs with
      |[] ->
        raise (Error
                 (Printf.sprintf
                    "Incorrect argument count in call (string>=? %s)"
                    (show_list show_exp_val vs)))
      |v1 :: vs' ->
        begin
          match v1 with
          |String s1 ->
            let rec visit_str_ge vs s' a =
              begin
                match vs with
                |[] ->
                  Boolean a
                |v :: vs' ->
                  begin
                    match v with
                    |String s ->
                      if s <= s'
                      then visit_str_ge vs' s a
                      else
                        if a
                        then visit_str_ge vs' s false
                        else
                          visit_str_ge vs' s a
                    |_ ->
                      raise (Error
                               (Printf.sprintf
                                  "Error in string>=?: %s is not a string."
                                  (show_exp_val v)))
                  end
              end
            in visit_str_ge vs' s1 true
          |_ ->
            raise (Error
                     (Printf.sprintf
                        "Error in string>=?: %s is not a string."
                        (show_exp_val v1)))
        end
    end)

let internal_str_lt = 
  (fun vs ->
    begin
      match vs with
      |[] ->
        raise (Error
                 (Printf.sprintf
                    "Incorrect argument count in call (string<? %s)"
                    (show_list show_exp_val vs)))
      |v1 :: vs' ->
        begin
          match v1 with
          |String s1 ->
            let rec visit_str_lt vs s' a =
              begin
                match vs with
                |[] ->
                  Boolean a
                |v :: vs' ->
                  begin
                    match v with
                    |String s ->
                      if s > s'
                      then visit_str_lt vs' s a
                      else
                        if a
                        then visit_str_lt vs' s false
                        else
                          visit_str_lt vs' s a
                    |_ ->
                      raise (Error
                               (Printf.sprintf
                                  "Error in string<?: %s is not a string."
                                  (show_exp_val v)))
                  end
              end
            in visit_str_lt vs' s1 true
          |_ ->
            raise (Error
                     (Printf.sprintf
                        "Error in string<?: %s is not a string."
                        (show_exp_val v1)))
        end
    end)


let internal_str_le = 
  (fun vs ->
    begin
      match vs with
      |[] ->
        raise (Error
                 (Printf.sprintf
                    "Incorrect argument count in call (string<=? %s)"
                    (show_list show_exp_val vs)))
      |v1 :: vs' ->
        begin
          match v1 with
          |String s1 ->
            let rec visit_str_le vs s' a =
              begin
                match vs with
                |[] ->
                  Boolean a
                |v :: vs' ->
                  begin
                    match v with
                    |String s ->
                      if s >= s'
                      then visit_str_le vs' s a
                      else
                        if a
                        then visit_str_le vs' s false
                        else
                          visit_str_le vs' s a
                    |_ ->
                      raise (Error
                               (Printf.sprintf
                                  "Error in string<=?: %s is not a string."
                                  (show_exp_val v)))
                  end
              end
            in visit_str_le vs' s1 true
          |_ ->
            raise (Error
                     (Printf.sprintf
                        "Error in string<=?: %s is not a string."
                        (show_exp_val v1)))
        end
    end)

let internal_str_length =   
  (fun vs ->
    begin
      match vs with
      |v :: [] ->
        begin
          match v with
          |String s ->
            let len = String.length s in
            Int len
          |_ ->
            raise (Error
                     (Printf.sprintf
                        "Error in string-length?: %s is not a string."
                        (show_exp_val v)))
        end
      |_ ->
        raise (Error
                 (Printf.sprintf
                    "Incorrect argument count in call (string-length? %s)"
                    (show_list show_exp_val vs)))
    end)

let internal_char_to_str =  
  (fun vs ->
    begin
      match vs with
      |[] ->
        raise (Error
                 (Printf.sprintf
                    "Incorrect argument count in call (string %s)"
                    (show_list show_exp_val vs)))
      |_ ->
        let rec make_str_from_chars vs =
          begin
            match vs with
            |[] -> []
            |v :: vs' ->
              begin
                match v with
                |Character c ->
                  (String.make 1 c) :: make_str_from_chars vs'
                |_ ->
                  raise (Error
                           (Printf.sprintf
                              "Error in string: %s is not a character."
                              (show_exp_val v)))
              end
          end
        in String (String.concat "" (make_str_from_chars vs))
    end)

let internal_str_ref =
  (fun vs ->
    begin
      match vs with
      |v1 :: v2 :: [] ->
        begin
          match v1 with
          |String s ->
            begin
              match v2 with
              |Int n ->
                let len = String.length s in
                if n < len
                then Character (String.get s n)
                else
                  raise (Error
                           (Printf.sprintf
                              "Error in string-ref: %s is not a valid index for %s."
                              (show_exp_val v2)(show_exp_val v1)))
              |_ ->
                raise (Error
                         (Printf.sprintf
                            "Error in string-ref: %s is not a valid index for %s."
                            (show_exp_val v2)(show_exp_val v1)))
            end
          |_ ->
            raise (Error
                     (Printf.sprintf
                        "Error in string-ref: %s is not a string."
                        (show_exp_val v1)))
        end
       
       
      |_ ->
        raise (Error
                 (Printf.sprintf
                    "Incorrect argument count in call (string-ref %s)"
                    (show_list show_exp_val vs)))
    end)

    (* Change error message such that the calling function name is also present *)

     (* Collecting all error messages into one error function *)
     
     (* Mutable list vs immutable list, circularity of lists can be present due to the mutability of the list elements. Implement the list length, append functions as library functions inside the Scheme environment. *)
     
     (* More efficient interpreter will avoid the construction of list in terms of the arguments taken in (i.e. fun vs). Can write about the possible optimizations to be made to the primitive functions, i.e. implement primitives with determined aritiesto factorize the commonality of the primitives that checks the type and returns true for one argument, etc.  *)

     (* Since pairs are mutable, the semantics for exp_val has to be exp_val ref * exp_val ref for Pairs *)

     (* Quotations are immutable !*)

     (* Consider whether our pairs should be mutable or immutable, because there is the case of circularity detectors if our pairs are mutable *)

     (* Anything the interpreter does not use, don't need to implement. I.e. mutable strings, mutable lists/pairs. *)

     (* Art of self-interpreter, make what you need smaller and smaller. What is needed to implement the self-interpreter is less and less, make it simpler. *)

     (* SExp Lib, SExp conversion in Jane Street *)
