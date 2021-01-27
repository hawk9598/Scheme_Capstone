open Primitives
open Ast
open Unparser

exception Error of string
                 
(* Environment *)

(* Define environment as list of arrays based on block *)
                 
(* Defining the environment as an association list alongside its functions. *)

(* The environment is a list of triplets, with the bool denoting whether the denotation name comes from extend-rec. Used for recursive purposes *)
         
type env = (name * bool * exp_val) list

let empty_alist = []
         
let default_empty_alist =
  [ ("pair?", false, Primitive internal_is_pair);
    ("cons", false, Primitive internal_cons);
    ("car", false, Primitive internal_car);
    ("cdr", false, Primitive internal_cdr);
    ("integer?", false, Primitive internal_is_int);
    ("zero?", false, Primitive internal_is_zero);
    ("positive?", false, Primitive internal_is_positive);
    ("negative?", false, Primitive internal_is_negative);
    ("even?", false, Primitive internal_is_even);
    ("odd?", false, Primitive internal_is_odd);
    ("+", false, Primitive internal_add);
    ("-", false, Primitive internal_sub);
    ("*", false, Primitive internal_mul);
    ("/", false, Primitive internal_div);
    ("quotient", false, Primitive internal_quotient);
    ("remainder", false, Primitive internal_remainder);
    ("expt", false, Primitive internal_exponentiation);
    ("<", false, Primitive internal_lt);
    ("<=", false, Primitive internal_lte);
    (">", false, Primitive internal_gt);
    (">=", false, Primitive internal_gte);
    ("=", false, Primitive internal_equal);
    ("char?", false, Primitive internal_is_char);
    ("char=?", false, Primitive internal_char_equal);
    ("char>?", false, Primitive internal_char_gt);
    ("char>=?", false, Primitive internal_char_ge);
    ("char<?", false, Primitive internal_char_lt);
    ("char<=?", false, Primitive internal_char_le);
    ("char-numeric?", false, Primitive internal_char_numeric);
    ("char-alphabetic?", false, Primitive internal_char_alphabetic);
    ("string?", false, Primitive internal_is_str);
    ("string=?", false, Primitive internal_str_equal);
    ("string>?", false, Primitive internal_str_gt);
    ("string>=?", false, Primitive internal_str_ge);
    ("string<?", false, Primitive internal_str_lt);
    ("string<=", false, Primitive internal_str_le);
    ("string-length?", false, Primitive internal_str_length);
    ("string", false, Primitive internal_char_to_str);
    ("string-ref", false, Primitive internal_str_ref)
  ]

(* Defining functions that help to extend the environment bindings *)

(* Defining the normal extend functions for non-recursive usage. *)
  
let extend_alist x d env =
  (x, false, d) :: env


let extend_alist_star xs_given vs_given env =
  let rec visit xs vs = 
    match xs with
    |[] ->
      begin match vs with
      |[] -> env
      |_ -> 
        raise (Error
                 (Printf.sprintf
                    "Arity mismatch, %s  %s"
                    (show_list show_string xs_given)
                    (show_list show_exp_val vs_given)))
      end
    |x :: xs' ->
      begin match vs with
      |[] ->
        raise (Error
                 (Printf.sprintf
                    "Arity mismatch,  %s  %s"
                    (show_list show_string xs_given)
                    (show_list show_exp_val vs_given)))
      |v :: vs' ->
        (x , false, v) :: visit xs' vs'
      end
  in visit xs_given vs_given;;

(* Defining the extend-rec versions of the above two extend functions *)


let extend_rec x d env =
  (x, true, d) :: env 

(* Defining the Lookup function *)

(* Change definiton of lookup function such that it treats extend-rec stuff differently *)

let rec lookup (x:name) (c: env) : exp_val =
  begin
    match c with
    |[] ->
      raise Not_found
    |l :: ls' ->
      begin
        match l with
        |(y, b, z) ->
          if x = y then
            if b
            then
              begin
                match z with
                |Recursive_closure f ->
                  Closure (f (Recursive_closure f))
                |_ -> failwith "should not occur."
              end
            else
              z
          else lookup x ls'
      end
  end

    
