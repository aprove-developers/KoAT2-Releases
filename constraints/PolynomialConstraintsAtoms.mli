open Batteries
open ID

type var = Variables.MakeVariableTerm(StringID).t
type t =
    GreaterThan of Polynomials.t * Polynomials.t
  | GreaterEqual of Polynomials.t * Polynomials.t
  | LessThan of Polynomials.t * Polynomials.t
  | LessEqual of Polynomials.t * Polynomials.t
  | Neq of Polynomials.t * Polynomials.t
  | Equal of Polynomials.t * Polynomials.t
val get_first_arg : t -> Polynomials.t
val get_second_arg : t -> Polynomials.t
val mk_gt : Polynomials.t -> Polynomials.t -> t
val mk_ge : Polynomials.t -> Polynomials.t -> t
val mk_lt : Polynomials.t -> Polynomials.t -> t
val mk_le : Polynomials.t -> Polynomials.t -> t
val mk_eq : Polynomials.t -> Polynomials.t -> t
val mk_neq : Polynomials.t -> Polynomials.t -> t
val is_gt : t -> bool
val is_ge : t -> bool
val is_lt : t -> bool
val is_le : t -> bool
val is_eq : t -> bool
val is_neq : t -> bool
val is_same_constr : t -> t -> bool
val simplify : t -> t
val equal : t -> t -> bool
val one : Polynomials.t
val remove_strictness : t -> t
val to_string : t -> string
val to_z3 : Z3.context -> t -> Z3.Expr.expr
val get_variables : t -> var list
val rename_vars : Variables.StringVariableTerm.rename_map -> t -> t
val instantiate_with_big_int : Variables.StringVariableTerm.valuation -> t -> bool
