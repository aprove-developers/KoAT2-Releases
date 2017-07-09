open Batteries
open ID

type t
type valuation = Variables.StringVariableTerm.valuation
type value = Variables.StringVariableTerm.value
val make : value -> Monomials.MakeMonomial(StringID).t -> t
val to_z3 : Z3.context -> t -> Z3.Expr.expr
val coeff : t -> value
val monomial : t -> Monomials.MakeMonomial(StringID).t
val degree : t -> int
val simplify : t -> t
val to_string_simplified : t -> string
val to_string : t -> string
val (==) : t -> t -> bool
val rename : Variables.StringVariableTerm.rename_map -> t -> t
val eval : t -> valuation -> value
val mult_with_const : value -> t -> t
val mult : t -> t -> t
