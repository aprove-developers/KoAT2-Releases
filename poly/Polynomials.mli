type t = ScaledMonomials.t list
type value = Big_int.big_int
val get_degree : t -> int
val to_z3 : Z3.context -> t -> Z3.Expr.expr
val get_coeff : Monomials.t -> t -> value
val delete_monomial : Monomials.t -> t -> t
val simplify_partial_simplified : t -> t
val simplify : t -> t
val to_string_simplified : t -> string
val to_string : t -> string
val equal_simplified : t -> t -> bool
val equal : t -> t -> bool
val get_monomials : t -> Monomials.t list
val from_var : Variables.t -> t
val zero : t
val one : t
val get_constant : t -> value
val from_constant : value -> t
val get_variables : t -> Variables.t list
val is_var : t -> bool
val is_var_plus_constant : t -> bool
val is_sum_of_vars_plus_constant : t -> bool
val is_univariate_and_linear : t -> bool
val is_const : t -> bool
val is_linear : t -> bool
val rename_vars : string Mapping.VarMap.t -> t -> t
val mult_with_const : value -> t -> t
val negate : t -> t
val add : t -> t -> t
val add_list : t list -> t
val subtract : t -> t -> t
val mult : t -> t -> t
val pow_poly : t -> int -> t
val eval : value Mapping.VarMap.t -> t -> value
