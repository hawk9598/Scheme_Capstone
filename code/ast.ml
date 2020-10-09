type name = string
          
type arg_name = Arg of name
                     
type exp =
  |Integer of int (* Integer *)
  |Bool of bool (* boolean *)
  |Var of name (* variable *) (* Names denoting predefine procedures *)
  |Str of string (* string *)
  |Char of char (* character *)
  |If of exp * exp * exp (* if expression *)
  (* |Quote of quotation (* Quote expression *) *)
  |Apply of exp * exp list (* Apply expression *)
  |Let_rec of (name * lambda_abstraction) list * exp (* Let rec expression *)
  |Lambda_abstraction of lambda_abstraction (* Lambda abstraction *)
and lambda_abstraction =
  |Lambda of lambda_formals * exp (* Lambda takes a single argument *) (* closure will just take exp val and not exp val list *)
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
  |Pair of exp_val * exp_val
  |Closure of (exp_val list -> exp_val)
  |Primitive of (exp_val list -> exp_val)(* string * (exp_val -> exp_val) *) 
(* type quotation =
    |Integer of int
    |Bool of bool
    |Str of string
    |Char of char
    |()
    |Cmpd_Quotation of quotation * quotation *) 
