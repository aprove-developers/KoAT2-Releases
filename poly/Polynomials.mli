type polynomial = ScaledMonomials.scaled_mon list
val get_degree : polynomial -> int
val to_z3 : Z3.context -> polynomial -> Z3.Expr.expr
val get_coeff : Monomials.monomial -> polynomial -> Big_int.big_int
val delete_monomial :
  Monomials.monomial -> polynomial -> ScaledMonomials.scaled_mon list
val simplify_partial_simplified :
  polynomial -> ScaledMonomials.scaled_mon list
val simplify : polynomial -> ScaledMonomials.scaled_mon list
val to_string_simplified : polynomial -> string
val to_string : polynomial -> string
val equal_simplified : polynomial -> polynomial -> bool
val equal : polynomial -> polynomial -> bool
val get_monomials : polynomial -> Monomials.monomial list
val from_var : Variables.variable -> ScaledMonomials.scaled_mon list
val zero : 'a list
val one : ScaledMonomials.scaled_mon list
val get_constant : polynomial -> Big_int.big_int
val from_constant : Big_int.big_int -> ScaledMonomials.scaled_mon list
val get_variables : polynomial -> Variables.variable list
val is_var : polynomial -> bool
val is_var_plus_constant : polynomial -> bool
val is_sum_of_vars_plus_constant : polynomial -> bool
val is_univariate_and_linear : polynomial -> bool
val is_const : polynomial -> bool
val is_linear : polynomial -> bool
val rename_vars :
  string Mapping.VarMap.t -> polynomial -> ScaledMonomials.scaled_mon list
val mult_with_const :
  Big_int.big_int -> polynomial -> ScaledMonomials.scaled_mon list
val negate : polynomial -> ScaledMonomials.scaled_mon list
val add : polynomial -> polynomial -> ScaledMonomials.scaled_mon list
val add_list : polynomial list -> ScaledMonomials.scaled_mon list
val subtract : polynomial -> polynomial -> ScaledMonomials.scaled_mon list
val mult : polynomial -> polynomial -> polynomial
val pow_poly : polynomial -> int -> polynomial
val instantiate_with_big_int :
  Big_int.big_int Mapping.VarMap.t -> polynomial -> Big_int.big_int
