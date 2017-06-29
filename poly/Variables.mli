type variable = string
val mk_var : string -> string
val to_string : variable -> variable
val varlist_to_string : variable list -> string
val equal : variable -> variable -> bool
val to_z3 : Z3.context -> variable -> Z3.Expr.expr
val get_new_var_name : string Mapping.VarMap.t -> variable -> variable
val instantiate_with_big_int :
  Big_int.big_int Mapping.VarMap.t -> variable -> Big_int.big_int
