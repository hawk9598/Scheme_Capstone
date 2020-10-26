open Primitives
open Ast
open Unparser

exception Error of string
                 
(* Environment *)
   
(* Defining the environment as an association list alongside its functions. *)
type env = (name * exp_val) list

let empty_alist = []
         
let default_empty_alist =
  [ ("pair?", Primitive internal_is_pair);
    ("car", Primitive internal_car);
    ("cdr", Primitive internal_cdr);
    ("integer?", Primitive internal_is_int);
    ("zero?", Primitive internal_is_zero);
    ("positive?", Primitive internal_is_positive);
    ("negative?", Primitive internal_is_negative);
    ("even?", Primitive internal_is_even);
    ("odd?", Primitive internal_is_odd);
    ("+", Primitive internal_add);
    ("-", Primitive internal_sub);
    ("*", Primitive internal_mul);
    ("/", Primitive internal_div);
    ("quotient", Primitive internal_quotient);
    ("remainder", Primitive internal_remainder);
    ("expt", Primitive internal_exponentiation);
    ("<", Primitive internal_lt);
    ("<=", Primitive internal_lte);
    (">", Primitive internal_gt);
    (">=", Primitive internal_gte);
    ("=", Primitive internal_equal)
  ]

let extend_alist x d env =
  (x, d) :: env


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
      raise Not_found
    |l :: ls' ->
      begin
        match l with
        |(y, z) ->
          if x = y then z
          else lookup x ls'
      end
  end

    
