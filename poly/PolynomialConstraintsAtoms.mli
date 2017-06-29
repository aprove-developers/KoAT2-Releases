(*basic constraints for polynomials*)
type constraint_atom =
    GreaterThan of Polynomials.polynomial * Polynomials.polynomial
  | GreaterEqual of Polynomials.polynomial * Polynomials.polynomial
  | LessThan of Polynomials.polynomial * Polynomials.polynomial
  | LessEqual of Polynomials.polynomial * Polynomials.polynomial
  | Neq of Polynomials.polynomial * Polynomials.polynomial
  | Equal of Polynomials.polynomial * Polynomials.polynomial
val mk_greater_than :
  Polynomials.polynomial -> Polynomials.polynomial -> constraint_atom
val mk_greater_equal :
  Polynomials.polynomial -> Polynomials.polynomial -> constraint_atom
val mk_less_than :
  Polynomials.polynomial -> Polynomials.polynomial -> constraint_atom
val mk_less_equal :
  Polynomials.polynomial -> Polynomials.polynomial -> constraint_atom
val mk_equal :
  Polynomials.polynomial -> Polynomials.polynomial -> constraint_atom
val mk_neq :
  Polynomials.polynomial -> Polynomials.polynomial -> constraint_atom
val to_string : constraint_atom -> string
val to_z3 : Z3.context -> constraint_atom -> Z3.Expr.expr
