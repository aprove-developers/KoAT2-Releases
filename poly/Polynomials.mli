open Batteries
open ID
   
type var = Variables.MakeVariableTerm(StringID).t
type t = ScaledMonomials.MakeScaledMonomial(StringID).t list
type valuation = Variables.MakeVariableTerm(StringID).valuation
type value = Variables.MakeVariableTerm(StringID).value
val degree : t -> int
val to_z3 : Z3.context -> t -> Z3.Expr.expr
val coeff : Monomials.MakeMonomial(StringID).t -> t -> value
val delete_monomial : Monomials.MakeMonomial(StringID).t -> t -> t
val simplify_partial_simplified : t -> t
val simplify : t -> t
val to_string_simplified : t -> string
val to_string : t -> string
val equal_simplified : t -> t -> bool
val equal : t -> t -> bool
val monomials : t -> Monomials.MakeMonomial(StringID).t list
val from_var : var -> t
val zero : t
val one : t
val constant : t -> value
val from_constant : value -> t
val vars : t -> var list
val is_var : t -> bool
val is_var_plus_constant : t -> bool
val is_sum_of_vars_plus_constant : t -> bool
val is_univariate_linear : t -> bool
val is_const : t -> bool
val is_linear : t -> bool
val rename : Variables.StringVariableTerm.rename_map -> t -> t
val mult_with_const : value -> t -> t
val negate : t -> t
val add : t -> t -> t
val sum : t list -> t
val subtract : t -> t -> t
val mult : t -> t -> t
val pow : t -> int -> t
val eval : t -> valuation -> value
