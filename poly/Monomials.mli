type monomial = Powers.pow list
type value = Big_int.big_int
val to_z3 : Z3.context -> monomial -> Z3.Expr.expr
val mk_mon : (Variables.variable * int) list -> monomial
val get_variables : monomial -> Variables.variable list
val get_degree : monomial -> int
val get_degree_variable : Variables.variable -> monomial -> int
val delete_var : Variables.variable -> monomial -> Powers.pow list
val simplify : monomial -> monomial
val to_string_simplified : monomial -> string
val to_string : monomial -> string
val equal_simplified : monomial -> monomial -> bool
val equal : monomial -> monomial -> bool
val is_univariate_linear_monomial : monomial -> bool
val rename_monomial : string Mapping.VarMap.t -> monomial -> monomial
val mult : monomial -> monomial -> monomial
val eval : value Mapping.VarMap.t -> monomial -> value
