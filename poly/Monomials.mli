type var = Variables.StringVariableTerm.t
type valuation = Variables.StringVariableTerm.valuation
type t = Powers.t list
type value = Variables.StringVariableTerm.value
val to_z3 : Z3.context -> t -> Z3.Expr.expr
val mk_mon : (var * int) list -> t
val get_variables : t -> var list
val get_degree : t -> int
val get_degree_variable : var -> t -> int
val delete_var : var -> t -> Powers.t list
val simplify : t -> t
val to_string_simplified : t -> string
val to_string : t -> string
val equal_simplified : t -> t -> bool
val equal : t -> t -> bool
val is_univariate_linear_monomial : t -> bool
val rename_monomial : Variables.StringVariableTerm.rename_map -> t -> t
val mult : t -> t -> t
val eval : valuation -> t -> value
