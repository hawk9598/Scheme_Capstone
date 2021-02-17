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
  (* |Quote of quotation (* Quote expression *) *)
  |Lambda_abstraction of lambda_abstraction (* Lambda abstraction *)
  |Apply of exp * exp list (* Apply expression *)
and lambda_abstraction =
  |Lambda of lambda_formals * exp
and lambda_formals =
  |Args_list of name list
  (* For (lambda (x y z. extras) ... ) *)
  |Args_improper_list of name list * name
  (* For variadic procedures *)
  |Args of name


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
  |Closure of (exp_val list -> (exp_val -> exp_val) -> exp_val)
  |Primitive of (exp_val list -> exp_val)(* string * (exp_val -> exp_val) *)
  (* Can remove this from exp_val and the unparser_cps.ml *)
  |APPLY 
  |CWCC (* Var "CWCC" is bound to this in the initial environment *)
  |Null (* empty list *)
  |Recursive_closure of recur_star
and recur_star =
  |Recur_star of (recur_star -> exp_val list -> (exp_val -> exp_val) -> exp_val) list
  
                                    
(* type quotation =
  |Int of int
  |Boolean of bool
  |String of string
  |Character of char
  |Null
  |Pair of exp_val * exp_val (* but quoted pairs are NOT mutable *) 
  |Cmpd_Quotation of quotation * quotation *) 
