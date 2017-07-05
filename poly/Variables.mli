type t = string
type value = Big_int.big_int
val mk_var : string -> t
val to_string : t -> string
val varlist_to_string : t list -> string
val equal : t -> t -> bool
val to_z3 : Z3.context -> t -> Z3.Expr.expr
val get_new_var_name : string Mapping.VarMap.t -> t -> t
val eval : value Mapping.VarMap.t -> t -> value
val equal_varlist : t list -> t list -> bool