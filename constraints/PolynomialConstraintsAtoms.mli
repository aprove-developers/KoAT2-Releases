type constraint_atom =
  |GreaterThan of Polynomials.t * Polynomials.t
  | GreaterEqual of Polynomials.t * Polynomials.t
  | LessThan of Polynomials.t * Polynomials.t
  | LessEqual of Polynomials.t * Polynomials.t
  | Neq of Polynomials.t * Polynomials.t
  | Equal of Polynomials.t * Polynomials.t
val get_first_arg : constraint_atom -> Polynomials.t
val get_second_arg : constraint_atom -> Polynomials.t
val mk_gt : Polynomials.t -> Polynomials.t -> constraint_atom
val mk_ge : Polynomials.t -> Polynomials.t -> constraint_atom
val mk_lt : Polynomials.t -> Polynomials.t -> constraint_atom
val mk_le : Polynomials.t -> Polynomials.t -> constraint_atom
val mk_eq : Polynomials.t -> Polynomials.t -> constraint_atom
val mk_neq : Polynomials.t -> Polynomials.t -> constraint_atom
val one : ScaledMonomials.scaled_mon list
val remove_strictness : constraint_atom -> constraint_atom
val to_string : constraint_atom -> string
val to_z3 : Z3.context -> constraint_atom -> Z3.Expr.expr
val get_variables : constraint_atom -> Variables.t list
val rename_vars : string Mapping.VarMap.t -> constraint_atom -> constraint_atom
val instantiate_with_big_int : Big_int.big_int Mapping.VarMap.t -> constraint_atom -> bool
