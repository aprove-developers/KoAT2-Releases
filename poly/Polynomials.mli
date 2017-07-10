open Batteries
open ID
   
type var = Variables.MakeVariableTerm(StringID).t
type t = ScaledMonomials.MakeScaledMonomial(StringID).t list
type valuation = Variables.MakeVariableTerm(StringID).valuation
type value = Variables.MakeVariableTerm(StringID).value
val get_degree : t -> int
val to_z3 : Z3.context -> t -> Z3.Expr.expr
val get_coeff : Monomials.MakeMonomial(StringID).t -> t -> value
val delete_monomial : Monomials.MakeMonomial(StringID).t -> t -> t
val simplify_partial_simplified : t -> t
val simplify : t -> t
val to_string_simplified : t -> string
val to_string : t -> string
val equal_simplified : t -> t -> bool
val equal : t -> t -> bool
val get_monomials : t -> Monomials.MakeMonomial(StringID).t list
val from_var : var -> t
val zero : t
val one : t
val get_constant : t -> value
val from_constant : value -> t
val get_variables : t -> var list
val is_var : t -> bool
val is_var_plus_constant : t -> bool
val is_sum_of_vars_plus_constant : t -> bool
val is_univariate_and_linear : t -> bool
val is_const : t -> bool
val is_linear : t -> bool
val rename_vars : Variables.StringVariableTerm.rename_map -> t -> t
val mult_with_const : value -> t -> t
val negate : t -> t
val add : t -> t -> t
val add_list : t list -> t
val subtract : t -> t -> t
val mult : t -> t -> t
val pow_poly : t -> int -> t
val eval : valuation -> t -> value
