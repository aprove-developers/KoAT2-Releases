open Batteries
open ID

(*Polynomial Constraints of the form p1<p2, p1<=p2, etc. Conjunctions of these constraints form the real constraints*)
type var = Variables.MakeVariableTerm(StringID).t
module Polynomial = Polynomials.MakePolynomial(StringID)
type polynomial = Polynomial.t

type t = 
    |GreaterThan of Polynomial.t * Polynomial.t
    |GreaterEqual of Polynomial.t * Polynomial.t
    |LessThan of Polynomial.t * Polynomial.t
    |LessEqual of Polynomial.t * Polynomial.t
    |Neq of Polynomial.t * Polynomial.t
    |Equal of Polynomial.t * Polynomial.t
    
let get_first_arg (comp : t) =
    match comp with
    |GreaterThan(p1, p2) | GreaterEqual (p1, p2) | LessThan (p1, p2) | LessEqual (p1, p2) | Neq (p1, p2) | Equal (p1, p2)-> p1

let get_second_arg (comp : t) =
    match comp with
    |GreaterThan(p1, p2) | GreaterEqual (p1, p2) | LessThan (p1, p2) | LessEqual (p1, p2) | Neq (p1, p2) | Equal (p1, p2)-> p2
    
let mk_gt (poly1 : Polynomial.t) (poly2 : Polynomial.t) =
    GreaterThan(poly1, poly2)

let mk_ge (poly1 : Polynomial.t) (poly2 : Polynomial.t) =
    GreaterEqual(poly1, poly2)

let mk_lt (poly1 : Polynomial.t) (poly2 : Polynomial.t) =
    LessThan(poly1, poly2)

let mk_le (poly1 : Polynomial.t) (poly2 : Polynomial.t) =
    LessEqual(poly1, poly2)

let mk_eq (poly1 : Polynomial.t) (poly2 : Polynomial.t) =
    Equal(poly1, poly2)
    
let mk_neq (poly1 : Polynomial.t) (poly2 : Polynomial.t) =
    Neq(poly1, poly2)

let is_gt (atom : t) =
    match atom with
        |GreaterThan(_, _) -> true
        |_ ->false

let is_ge (atom : t) =
    match atom with
        |GreaterEqual(_, _) -> true
        |_ ->false

let is_lt (atom : t) =
    match atom with
        |LessThan(_, _) -> true
        |_ ->false

let is_le (atom : t) =
    match atom with
        |LessEqual(_, _) -> true
        |_ ->false

let is_eq (atom : t) =
    match atom with
        |Equal(_, _) -> true
        |_ ->false
    
let is_neq (atom : t) =
    match atom with
        |Neq(_, _) -> true
        |_ ->false
        
let is_same_constr (atom1 : t) (atom2 : t) =
    match (atom1, atom2) with
     |(GreaterThan (_,_), GreaterThan(_,_)) -> true
     |(GreaterEqual (_,_), GreaterEqual(_,_)) -> true
     |(LessThan (_,_), LessThan(_,_)) -> true
     |(LessEqual (_,_), LessEqual(_,_)) -> true
     |(Equal (_,_), Equal (_,_)) -> true
     |(Neq (_,_), Neq (_,_)) -> true
     |(_,_) -> false
    
let simplify (atom : t) =
    let poly_simplify = Polynomial.simplify in
        match atom with
            |GreaterThan (p1, p2)-> mk_gt (poly_simplify p1) (poly_simplify p2)
            |GreaterEqual (p1, p2)-> mk_ge (poly_simplify p1) (poly_simplify p2)
            |LessThan (p1, p2)-> mk_lt (poly_simplify p1) (poly_simplify p2)
            |LessEqual (p1, p2)-> mk_le (poly_simplify p1) (poly_simplify p2)
            |Neq (p1, p2)-> mk_neq (poly_simplify p1) (poly_simplify p2)
            |Equal (p1, p2)-> mk_eq (poly_simplify p1) (poly_simplify p2)
            
let equal (atom1 : t) (atom2 : t) =
    let poly_equal = Polynomial.(==) in
        (is_same_constr atom1 atom2) && (poly_equal (get_first_arg atom1) (get_first_arg atom2)) && (poly_equal (get_second_arg atom1) (get_second_arg atom2))
    
let one = Polynomial.one

(* In this setting everything represents integer values. Hence strictness can be removed by adding/subtracting one*)

let remove_strictness (comp : t) =
    match comp with
    | GreaterThan (p1, p2)-> GreaterEqual (p1, (Polynomial.add p2 one))
    | LessThan (p1, p2)-> LessEqual( p1, (Polynomial.subtract p2 one))
    | _ -> comp
    
let to_string (comp : t) =
    match comp with
    |GreaterThan (p1, p2)-> String.concat " > " [Polynomial.to_string p1; Polynomial.to_string p2]
    |GreaterEqual (p1, p2)-> String.concat " >= " [Polynomial.to_string p1; Polynomial.to_string p2]
    |LessThan (p1, p2)-> String.concat " < " [Polynomial.to_string p1; Polynomial.to_string p2]
    |LessEqual (p1, p2)-> String.concat " <= " [Polynomial.to_string p1; Polynomial.to_string p2]
    |Neq (p1, p2)-> String.concat " != " [Polynomial.to_string p1; Polynomial.to_string p2]
    |Equal (p1, p2)-> String.concat " = " [Polynomial.to_string p1; Polynomial.to_string p2]
    
let to_z3 (ctx : Z3.context) (comp : t) =
    match comp with
    |GreaterThan (p1, p2)-> Z3.Arithmetic.mk_gt ctx (Polynomial.to_z3 ctx p1) (Polynomial.to_z3 ctx p2)
    |GreaterEqual (p1, p2)-> Z3.Arithmetic.mk_ge ctx (Polynomial.to_z3 ctx p1) (Polynomial.to_z3 ctx p2)
    |LessThan (p1, p2)-> Z3.Arithmetic.mk_lt ctx (Polynomial.to_z3 ctx p1) (Polynomial.to_z3 ctx p2)
    |LessEqual (p1, p2)-> Z3.Arithmetic.mk_le ctx (Polynomial.to_z3 ctx p1) (Polynomial.to_z3 ctx p2)
    |Equal (p1, p2)-> Z3.Boolean.mk_eq ctx (Polynomial.to_z3 ctx p1) (Polynomial.to_z3 ctx p2)
    |Neq (p1, p2)-> Z3.Boolean.mk_not ctx (Z3.Boolean.mk_eq ctx (Polynomial.to_z3 ctx p1) (Polynomial.to_z3 ctx p2))
    
let get_variables (comp : t) =
    List.unique (List.append (Polynomial.vars (get_first_arg comp)) (Polynomial.vars (get_second_arg comp)))
    
let rename_vars (varmapping : Variables.StringVariableTerm.rename_map) (comp : t) =
    match comp with
    |GreaterThan (p1, p2)-> GreaterThan ((Polynomial.rename varmapping p1), (Polynomial.rename varmapping p2))
    |GreaterEqual (p1, p2)-> GreaterEqual ((Polynomial.rename varmapping p1), (Polynomial.rename varmapping p2))
    |LessThan (p1, p2)-> LessThan ((Polynomial.rename varmapping p1), (Polynomial.rename varmapping p2))
    |LessEqual (p1, p2)-> LessEqual ((Polynomial.rename varmapping p1), (Polynomial.rename varmapping p2))
    |Neq (p1, p2)-> Neq ((Polynomial.rename varmapping p1), (Polynomial.rename varmapping p2))
    |Equal (p1, p2)-> Equal ((Polynomial.rename varmapping p1), (Polynomial.rename varmapping p2))

let instantiate_with_big_int (varmapping : Variables.StringVariableTerm.valuation) (comp : t) =
    match comp with
    |GreaterThan (p1, p2)-> (Big_int.gt_big_int (Polynomial.eval p1 varmapping) (Polynomial.eval p2 varmapping))
    |GreaterEqual (p1, p2)-> (Big_int.ge_big_int (Polynomial.eval p1 varmapping) (Polynomial.eval p2 varmapping))
    |LessThan (p1, p2)-> (Big_int.lt_big_int (Polynomial.eval p1 varmapping) (Polynomial.eval p2 varmapping))
    |LessEqual (p1, p2)-> (Big_int.le_big_int (Polynomial.eval p1 varmapping) (Polynomial.eval p2 varmapping))
    |Neq (p1, p2)-> not(Big_int.eq_big_int (Polynomial.eval p1 varmapping) (Polynomial.eval p2 varmapping))
    |Equal (p1, p2)-> (Big_int.eq_big_int (Polynomial.eval p1 varmapping) (Polynomial.eval p2 varmapping))
