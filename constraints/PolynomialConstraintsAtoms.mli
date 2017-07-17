open Batteries
open ID

type polynomial = StdPoly.Polynomial.t
type t =
    GreaterThan of polynomial * polynomial
  | GreaterEqual of polynomial * polynomial
  | LessThan of polynomial * polynomial
  | LessEqual of polynomial * polynomial
  | Neq of polynomial * polynomial
  | Equal of polynomial * polynomial
val get_first_arg : t -> polynomial
val get_second_arg : t -> polynomial
val mk_gt : polynomial -> polynomial -> t
val mk_ge : polynomial -> polynomial -> t
val mk_lt : polynomial -> polynomial -> t
val mk_le : polynomial -> polynomial -> t
val mk_eq : polynomial -> polynomial -> t
val mk_neq : polynomial -> polynomial -> t
val is_gt : t -> bool
val is_ge : t -> bool
val is_lt : t -> bool
val is_le : t -> bool
val is_eq : t -> bool
val is_neq : t -> bool
val is_same_constr : t -> t -> bool
val equal : t -> t -> bool
val remove_strictness : t -> t
val to_string : t -> string
val to_z3 : Z3.context -> t -> Z3.Expr.expr
val get_variables : t -> StdPoly.Polynomial.var list
val rename_vars : StdPoly.Polynomial.rename_map -> t -> t
val instantiate_with_big_int : StdPoly.Polynomial.valuation -> t -> bool
