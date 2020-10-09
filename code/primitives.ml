(* Define the primitive functions with PROPER error messages at top level. *)

open Ast
open Unparser
   
exception Error of string

(* Defining internal functions for pairs and lists *)
                 
let internal_is_pair =
  Primitive
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
                      "Incorrect argument count in call %s"
                      (show_list show_exp_val vs)))
      end)
  
let internal_car =
  Primitive
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
                      "Incorrect argument count in call %s"
                      (show_list show_exp_val vs)))
      end)
  
let internal_cdr =
  Primitive
    (fun vs ->
      begin
        match vs with
        |v :: [] ->
          begin
            match v with
            |Pair (_, v2) ->
              v2
            |_ ->
              raise (Error
                       (Printf.sprintf
                          "Error in car: %s is not a pair."
                          (show_exp_val v)))
          end

        |_ ->
          raise (Error
                   (Printf.sprintf
                      "Incorrect argument count in call %s"
                      (show_list show_exp_val vs)))
      end)

(* Defining internal functions for numerical operations *)

let internal_is_int =
  Primitive
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
                      "Incorrect argument count in call %s"
                      (show_list show_exp_val vs)))
      end)

let internal_is_zero =
  Primitive
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
                      "Incorrect argument count in call %s"
                      (show_list show_exp_val vs)))
      end)

let internal_is_positive =
  Primitive
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
                      "Incorrect argument count in call %s"
                      (show_list show_exp_val vs)))
      end)

let internal_is_negative =
  Primitive
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
                      "Incorrect argument count in call %s"
                      (show_list show_exp_val vs)))
      end)

let internal_is_even =
  Primitive
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
                      "Incorrect argument count in call %s"
                      (show_list show_exp_val vs)))
      end)

let internal_is_odd =
  Primitive
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
                      "Incorrect argument count in call %s"
                      (show_list show_exp_val vs)))
      end)  
  
let internal_add =
  Primitive
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
  Primitive
    (fun vs ->
      begin
        match vs with
        |[] ->
          raise (Error
                   (Printf.sprintf
                      "Incorrect argument count in call %s"
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
  Primitive
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
  Primitive
    (fun vs ->
      begin
        match vs with
        |[] ->
          raise (Error
                   (Printf.sprintf
                      "Incorrect argument count in call %s"
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
  Primitive
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
                      "Incorrect argument count in call %s"
                      (show_list show_exp_val vs)))
      end)

let internal_remainder =
  Primitive
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
                      "Incorrect argument count in call %s"
                      (show_list show_exp_val vs)))
      end)

let internal_exponentiation =
  Primitive
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
                  let rec power x n a =
                    if n == 0
                    then Int a
                    else
                      let n' = n - 1 in
                      power x n' (x * a)
                  in power i1 i2 1
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
                      "Incorrect argument count in call %s"
                      (show_list show_exp_val vs)))
      end)
          

let internal_lt =
  Primitive
    (fun vs ->
      begin
        match vs with
        |[] ->
          raise (Error
                   (Printf.sprintf
                      "Incorrect argument count in call %s"
                      (show_list show_exp_val vs)))
        |v :: [] ->
          begin
            match v with
            |Int _ ->
              Boolean true
            |_ ->
              raise (Error
                       (Printf.sprintf
                          "Error in <: %s is not a number."
                          (show_exp_val v)))
          end
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
                        Boolean false
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
  Primitive
    (fun vs ->
      begin
        match vs with
        |[] ->
          raise (Error
                   (Printf.sprintf
                      "Incorrect argument count in call %s"
                      (show_list show_exp_val vs)))
        |v :: [] ->
          begin
            match v with
            |Int _ ->
              Boolean true
            |_ ->
              raise (Error
                       (Printf.sprintf
                          "Error in <=: %s is not a number."
                          (show_exp_val v)))
          end
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
                        Boolean false
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
  Primitive
    (fun vs ->
      begin
        match vs with
        |[] ->
          raise (Error
                   (Printf.sprintf
                      "Incorrect argument count in call %s"
                      (show_list show_exp_val vs)))
        |v :: [] ->
          begin
            match v with
            |Int _ ->
              Boolean true
            |_ ->
              raise (Error
                       (Printf.sprintf
                          "Error in >: %s is not a number."
                          (show_exp_val v)))
          end
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
                        Boolean false
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
  Primitive
    (fun vs ->
      begin
        match vs with
        |[] ->
          raise (Error
                   (Printf.sprintf
                      "Incorrect argument count in call %s"
                      (show_list show_exp_val vs)))
        |v :: [] ->
          begin
            match v with
            |Int _ ->
              Boolean true
            |_ ->
              raise (Error
                       (Printf.sprintf
                          "Error in >=: %s is not a number."
                          (show_exp_val v)))
          end
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
                        Boolean false
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
   Primitive
     (fun vs ->
       begin
         match vs with
         |[] ->
           raise (Error
                    (Printf.sprintf
                       "Incorrect argument count in call %s"
                       (show_list show_exp_val vs)))
         |v :: [] ->
           begin
             match v with
             |Int _ ->
               Boolean true
             |_ ->
               raise (Error
                        (Printf.sprintf
                           "Error in =: %s is not a number."
                           (show_exp_val v)))
           end
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
                         Boolean false
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
   Primitive
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
                       "Incorrect argument count in call %s"
                       (show_list show_exp_val vs)))
       end)

 let internal_char_equal =
   Primitive
     (fun vs ->
       begin
         match vs with
         |[] ->
           raise (Error
                    (Printf.sprintf
                       "Incorrect argument count in call %s"
                       (show_list show_exp_val vs)))
         |v :: [] ->
           begin
             match v with
             |Character _ ->
               Boolean true
             |_ ->
               raise (Error
                        (Printf.sprintf
                           "Error in char=?: %s is not a character."
                           (show_exp_val v)))
           end
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
                         then visit_char_equal vs' c' a
                         else
                           Boolean false
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
