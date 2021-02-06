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
  |Let_rec of (name * lambda_abstraction) list * exp (* Let rec expression *)
  |Let_rec_unary of (name * lambda_abstraction) * exp (* Let rec expression unary *)
  (* |Quote of quotation (* Quote expression *) *)
  |Lambda_abstraction of lambda_abstraction (* Lambda abstraction *)
  |Apply of exp * exp list (* Apply expression *)
and lambda_abstraction =
  |Lambda of lambda_formals * exp
and lambda_formals =
  |Args_list of name list


type top_level_form =
  |Define of name * exp
  |Exp of exp

type prog =
  |Prog of top_level_form list

type exp_val =
  |Int of int
  |Boolean of bool
  |String of string
  |Character of char
  |Pair of exp_val * exp_val (* decide to make pairs immutable for now *)
  |Closure of (exp_val list -> exp_val)
  |Primitive of (exp_val list -> exp_val)(* string * (exp_val -> exp_val) *)
  |Null (* empty list *)
  |Recursive_closure of (exp_val -> exp_val list -> exp_val)
  |Recursive_closure_non_unary of recur_star
and recur_star =
  |Recur_star of (recur_star -> exp_val list -> exp_val) list
  
                                    
(* type quotation =
  |Int of int
  |Boolean of bool
  |String of string
  |Character of char
  |Null
  |Pair of exp_val * exp_val (* but quoted pairs are NOT mutable *) 
  |Cmpd_Quotation of quotation * quotation *)
