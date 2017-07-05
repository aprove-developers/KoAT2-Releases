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
val one : Polynomials.t
val remove_strictness : t -> t
val to_string : t -> string
val to_z3 : Z3.context -> t -> Z3.Expr.expr
val get_variables : t -> Variables.t list
val rename_vars :
  string Mapping.VarMap.t -> t -> t
val instantiate_with_big_int :
  Big_int.big_int Mapping.VarMap.t -> t -> bool
