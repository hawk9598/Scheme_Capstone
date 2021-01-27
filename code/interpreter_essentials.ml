open Primitives
open Ast
open Unparser

exception Error of string
exception Lookup_not_found of string
                 
(* Environment *)

(* Define environment as list of arrays based on block *)
                 
(* Defining the environment as an association list alongside its functions. *)
         
type env = (name * exp_val) list

let empty_alist = []
         
let default_empty_alist =
  [ ("pair?",  Primitive internal_is_pair);
    ("cons",  Primitive internal_cons);
    ("car",  Primitive internal_car);
    ("cdr",  Primitive internal_cdr);
    ("integer?",  Primitive internal_is_int);
    ("zero?", Primitive internal_is_zero);
    ("positive?", Primitive internal_is_positive);
    ("negative?",  Primitive internal_is_negative);
    ("even?",  Primitive internal_is_even);
    ("odd?",  Primitive internal_is_odd);
    ("+",  Primitive internal_add);
    ("-",  Primitive internal_sub);
    ("*", Primitive internal_mul);
    ("/", Primitive internal_div);
    ("quotient",  Primitive internal_quotient);
    ("remainder", Primitive internal_remainder);
    ("expt",  Primitive internal_exponentiation);
    ("<", Primitive internal_lt);
    ("<=",  Primitive internal_lte);
    (">",  Primitive internal_gt);
    (">=",  Primitive internal_gte);
    ("=",  Primitive internal_equal);
    ("char?", Primitive internal_is_char);
    ("char=?", Primitive internal_char_equal);
    ("char>?", Primitive internal_char_gt);
    ("char>=?", Primitive internal_char_ge);
    ("char<?", Primitive internal_char_lt);
    ("char<=?", Primitive internal_char_le);
    ("char-numeric?", Primitive internal_char_numeric);
    ("char-alphabetic?", Primitive internal_char_alphabetic);
    ("string?", Primitive internal_is_str);
    ("string=?", Primitive internal_str_equal);
    ("string>?", Primitive internal_str_gt);
    ("string>=?", Primitive internal_str_ge);
    ("string<?", Primitive internal_str_lt);
    ("string<=", Primitive internal_str_le);
    ("string-length?", Primitive internal_str_length);
    ("string",  Primitive internal_char_to_str);
    ("string-ref", Primitive internal_str_ref)
  ]

(* Defining functions that help to extend the environment bindings *)
  
let extend_alist x d env =
  (x,  d) :: env


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
        (x , v) :: visit xs' vs'
      end
  in visit xs_given vs_given;;

(* Defining the Lookup function *)

let rec lookup (x:name) (c: env) : exp_val =
  begin
    match c with
    |[] ->
      (* Add what is not found *)
      raise (Lookup_not_found x)
    |(y, z) :: c' ->
      if x = y then
        begin
          match z with
          |Recursive_closure f ->
            Closure (f (Recursive_closure f))
          |_ -> z
        end
      else
        lookup x c'
  end

    
