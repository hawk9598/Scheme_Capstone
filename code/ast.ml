type name = string
          
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
  |Symbol of name
and recur_star =
  |Recur_star of (recur_star -> exp_val list -> exp_val) list
