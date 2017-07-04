type constraint_atom =
    GreaterThan of Polynomials.polynomial * Polynomials.polynomial
  | GreaterEqual of Polynomials.polynomial * Polynomials.polynomial
  | LessThan of Polynomials.polynomial * Polynomials.polynomial
  | LessEqual of Polynomials.polynomial * Polynomials.polynomial
  | Neq of Polynomials.polynomial * Polynomials.polynomial
  | Equal of Polynomials.polynomial * Polynomials.polynomial
val get_first_arg : constraint_atom -> Polynomials.polynomial
val get_second_arg : constraint_atom -> Polynomials.polynomial
val mk_gt :
  Polynomials.polynomial -> Polynomials.polynomial -> constraint_atom
val mk_ge :
  Polynomials.polynomial -> Polynomials.polynomial -> constraint_atom
val mk_lt :
  Polynomials.polynomial -> Polynomials.polynomial -> constraint_atom
val mk_le :
  Polynomials.polynomial -> Polynomials.polynomial -> constraint_atom
val mk_eq :
  Polynomials.polynomial -> Polynomials.polynomial -> constraint_atom
val mk_neq :
  Polynomials.polynomial -> Polynomials.polynomial -> constraint_atom
val one : ScaledMonomials.scaled_mon list
val remove_strictness : constraint_atom -> constraint_atom
val to_string : constraint_atom -> string
val to_z3 : Z3.context -> constraint_atom -> Z3.Expr.expr
val get_variables : constraint_atom -> Variables.variable list
val rename_vars :
  string Mapping.VarMap.t -> constraint_atom -> constraint_atom
val instantiate_with_big_int :
  Big_int.big_int Mapping.VarMap.t -> constraint_atom -> bool
