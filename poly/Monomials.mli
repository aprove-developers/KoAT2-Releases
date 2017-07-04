type t = Powers.t list
type value = Big_int.big_int
val to_z3 : Z3.context -> t -> Z3.Expr.expr
val mk_mon : (Variables.t * int) list -> t
val get_variables : t -> Variables.t list
val get_degree : t -> int
val get_degree_variable : Variables.t -> t -> int
val delete_var : Variables.t -> t -> Powers.t list
val simplify : t -> t
val to_string_simplified : t -> string
val to_string : t -> string
val equal_simplified : t -> t -> bool
val equal : t -> t -> bool
val is_univariate_linear_monomial : t -> bool
val rename_monomial : string Mapping.VarMap.t -> t -> t
val mult : t -> t -> t
val eval : value Mapping.VarMap.t -> t -> value
