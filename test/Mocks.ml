module Polynomial =
  struct
    type t =
      | Constant of int
      | Variable of string
      | Neg of t
      | Plus of t * t
      | Times of t * t
      | Pow of t * int              
    let from_constant_int c = Constant c
    let from_var_string str = Variable str
    let zero = Constant 0
    let one = Constant 1
    let neg p = Neg p
    let add p1 p2 = Plus (p1, p2)
    let mul p1 p2 = Times (p1, p2)
    let pow p n = Pow (p, n)
    let rec to_string = function
      | Constant c -> string_of_int c
      | Variable v -> v
      | Neg t -> String.concat "" ["("; "-"; to_string t; ")"]
      | Plus (t1,t2) -> String.concat "" ["("; to_string t1; "+"; to_string t2; ")"]
      | Times (t1,t2) -> String.concat "" ["("; to_string t1; "*"; to_string t2; ")"]
      | Pow (p,n) -> String.concat "" ["("; to_string p; "^"; string_of_int n; ")"]
  end

module PolynomialConstraintAtom =
  struct
    module Polynomial_ = Polynomial
    type polynomial = Polynomial_.t
    type t =
      | Equal of polynomial * polynomial
      | Neq of polynomial * polynomial
      | LessThan of polynomial * polynomial
      | LessEqual of polynomial * polynomial
      | GreaterEqual of polynomial * polynomial
      | GreaterThan of polynomial * polynomial
    let mk_gt p1 p2 = GreaterThan(p1, p2)
    let mk_ge p1 p2 = GreaterEqual(p1, p2)
    let mk_lt p1 p2 = LessThan(p1, p2)
    let mk_le p1 p2 = LessEqual(p1, p2)
    let mk_eq p1 p2 = Equal(p1, p2)
    let mk_neq p1 p2 = Neq(p1, p2)
    let to_string c = match c with
      | Equal (p1, p2) -> String.concat " == " [(Polynomial.to_string p1); (Polynomial.to_string p2)]
      | Neq (p1, p2) -> String.concat " <> " [(Polynomial.to_string p1); (Polynomial.to_string p2)]
      | LessThan (p1, p2) -> String.concat " < " [(Polynomial.to_string p1); (Polynomial.to_string p2)]
      | LessEqual (p1, p2) -> String.concat " <= " [(Polynomial.to_string p1); (Polynomial.to_string p2)]
      | GreaterEqual (p1, p2) -> String.concat " >= " [(Polynomial.to_string p1); (Polynomial.to_string p2)]
      | GreaterThan (p1, p2) -> String.concat " > " [(Polynomial.to_string p1); (Polynomial.to_string p2)]
  end

module PolynomialConstraints = 
  struct
    module Polynomial_ = Polynomial
    module PolynomialConstraintsAtoms_= PolynomialConstraintAtom
    type polynomial = Polynomial_.t
    type atom = PolynomialConstraintsAtoms_.t
    type t = atom list
      
    let to_string c = String.concat " /\ " ( List.map PolynomialConstraintsAtoms_.to_string c)
    
    let mk atoms = atoms
      
  end
