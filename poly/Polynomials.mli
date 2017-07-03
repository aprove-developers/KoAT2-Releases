type polynomial = ScaledMonomials.t list
type value = Big_int.big_int
val get_degree : polynomial -> int
val to_z3 : Z3.context -> polynomial -> Z3.Expr.expr
val get_coeff : Monomials.t -> polynomial -> value
val delete_monomial : Monomials.t -> polynomial -> polynomial
val simplify_partial_simplified : polynomial -> polynomial
val simplify : polynomial -> polynomial
val to_string_simplified : polynomial -> string
val to_string : polynomial -> string
val equal_simplified : polynomial -> polynomial -> bool
val equal : polynomial -> polynomial -> bool
val get_monomials : polynomial -> Monomials.t list
val from_var : Variables.t -> polynomial
val zero : polynomial
val one : polynomial
val get_constant : polynomial -> value
val from_constant : value -> polynomial
val get_variables : polynomial -> Variables.t list
val is_var : polynomial -> bool
val is_var_plus_constant : polynomial -> bool
val is_sum_of_vars_plus_constant : polynomial -> bool
val is_univariate_and_linear : polynomial -> bool
val is_const : polynomial -> bool
val is_linear : polynomial -> bool
val rename_vars : string Mapping.VarMap.t -> polynomial -> polynomial
val mult_with_const : value -> polynomial -> polynomial
val negate : polynomial -> polynomial
val add : polynomial -> polynomial -> polynomial
val add_list : polynomial list -> polynomial
val subtract : polynomial -> polynomial -> polynomial
val mult : polynomial -> polynomial -> polynomial
val pow_poly : polynomial -> int -> polynomial
val eval : value Mapping.VarMap.t -> polynomial -> value
