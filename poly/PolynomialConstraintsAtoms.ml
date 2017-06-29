(*Polynomial Constraints of the form p1<p2, p1<=p2, etc. Conjunctions of these constraints form the real constraints*)
type constraint_atom = 
    |GreaterThan of Polynomials.polynomial * Polynomials.polynomial
    |GreaterEqual of Polynomials.polynomial * Polynomials.polynomial
    |LessThan of Polynomials.polynomial * Polynomials.polynomial
    |LessEqual of Polynomials.polynomial * Polynomials.polynomial
    |Neq of Polynomials.polynomial * Polynomials.polynomial
    |Equal of Polynomials.polynomial * Polynomials.polynomial
    
let mk_greater_than (poly1 : Polynomials.polynomial) (poly2 : Polynomials.polynomial) =
    GreaterThan(poly1, poly2)

let mk_greater_equal (poly1 : Polynomials.polynomial) (poly2 : Polynomials.polynomial) =
    GreaterEqual(poly1, poly2)

let mk_less_than (poly1 : Polynomials.polynomial) (poly2 : Polynomials.polynomial) =
    LessThan(poly1, poly2)

let mk_less_equal (poly1 : Polynomials.polynomial) (poly2 : Polynomials.polynomial) =
    LessEqual(poly1, poly2)

let mk_equal (poly1 : Polynomials.polynomial) (poly2 : Polynomials.polynomial) =
    Equal(poly1, poly2)
    
let mk_neq (poly1 : Polynomials.polynomial) (poly2 : Polynomials.polynomial) =
    Neq(poly1, poly2)
    
let to_string (comp : constraint_atom) =
    match comp with
    |GreaterThan (p1, p2)-> String.concat ">" [Polynomials.to_string p1; Polynomials.to_string p2]
    |GreaterEqual (p1, p2)-> String.concat ">=" [Polynomials.to_string p1; Polynomials.to_string p2]
    |LessThan (p1, p2)-> String.concat "<" [Polynomials.to_string p1; Polynomials.to_string p2]
    |LessEqual (p1, p2)-> String.concat "<=" [Polynomials.to_string p1; Polynomials.to_string p2]
    |Neq (p1, p2)-> String.concat "<>" [Polynomials.to_string p1; Polynomials.to_string p2]
    |Equal (p1, p2)-> String.concat "<>" [Polynomials.to_string p1; Polynomials.to_string p2]
    
let to_z3 (ctx : Z3.context) (comp : constraint_atom) =
    match comp with
    |GreaterThan (p1, p2)-> Z3.Arithmetic.mk_gt ctx (Polynomials.to_z3 ctx p1) (Polynomials.to_z3 ctx p2)
    |GreaterEqual (p1, p2)-> Z3.Arithmetic.mk_ge ctx (Polynomials.to_z3 ctx p1) (Polynomials.to_z3 ctx p2)
    |LessThan (p1, p2)-> Z3.Arithmetic.mk_lt ctx (Polynomials.to_z3 ctx p1) (Polynomials.to_z3 ctx p2)
    |LessEqual (p1, p2)-> Z3.Arithmetic.mk_le ctx (Polynomials.to_z3 ctx p1) (Polynomials.to_z3 ctx p2)
    |Neq (p1, p2)-> Z3.Boolean.mk_eq ctx (Polynomials.to_z3 ctx p1) (Polynomials.to_z3 ctx p2)
    |Equal (p1, p2)-> Z3.Boolean.mk_not ctx (Z3.Boolean.mk_eq ctx (Polynomials.to_z3 ctx p1) (Polynomials.to_z3 ctx p2))