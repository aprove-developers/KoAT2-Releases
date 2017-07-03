type monomial = Powers.pow list
val to_z3 : Z3.context -> monomial -> Z3.Expr.expr
val mk_mon : (Variables.variable * int) list -> Powers.pow list
val get_variables : monomial -> Variables.variable list
val get_degree : monomial -> int
val get_degree_variable : Variables.variable -> monomial -> int
val delete_var : Variables.variable -> monomial -> Powers.pow list
val simplify : monomial -> Powers.pow list
val to_string_simplified : monomial -> string
val to_string : monomial -> string
val equal_simplified : monomial -> monomial -> bool
val equal : monomial -> monomial -> bool
val is_univariate_linear_monomial : monomial -> bool
val rename_monomial : string Mapping.VarMap.t -> monomial -> Powers.pow list
val mult : monomial -> monomial -> Powers.pow list
val instantiate_with_big_int :
  Big_int.big_int Mapping.VarMap.t -> monomial -> Big_int.big_int
