type name = string
          
type arg_name = Arg of name
                     
type exp =
  |Integer of int (* Integer *)
  |Bool of bool (* Boolean *)
  |Char of char (* Character *)
  |Str of string (* String *)
  |Var of name (* Variable *)
  |Var_rec of name * int (* Recursive Variables *)
  |If of exp * exp * exp (* If expression *)
  |Let of (name * exp) list * exp (* Let expression *)
  |Let_rec of (name * (lambda_formals * exp)) list * exp (* Let rec expression *)
  |Quote of exp (* Quote *)
  |Lambda_abstraction of lambda_formals * exp  (* Lambda abstraction *)
  |Apply of exp * exp list (* Apply expression *)
and lambda_formals =
  (* For (lambda (x y) ... ) *)
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
