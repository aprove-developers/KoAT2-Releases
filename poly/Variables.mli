type variable = string
type value = Big_int.big_int
val mk_var : string -> variable
val to_string : variable -> string
val varlist_to_string : variable list -> string
val equal : variable -> variable -> bool
val to_z3 : Z3.context -> variable -> Z3.Expr.expr
val get_new_var_name : string Mapping.VarMap.t -> variable -> variable
val eval : value Mapping.VarMap.t -> variable -> value
