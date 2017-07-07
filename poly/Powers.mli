type var = Variables.StringVariableTerm.t
type valuation = Variables.StringVariableTerm.valuation
type t = {
    var : var; 
    n : int
  }
type value = Variables.StringVariableTerm.value
val to_string : t -> string
val to_z3 : Z3.context -> t -> Z3.Expr.expr
val mk_pow_from_var : var -> int -> t
val mk_var : string -> t
val get_variable : t -> var
val get_degree : t -> int
val equal : t -> t -> bool
val rename_power : Variables.StringVariableTerm.rename_map -> t -> t
val eval : valuation -> t -> value
