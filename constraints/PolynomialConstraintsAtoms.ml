(*Polynomial Constraints of the form p1<p2, p1<=p2, etc. Conjunctions of these constraints form the real constraints*)
open Mapping

type constraint_atom = 
    |GreaterThan of Polynomials.polynomial * Polynomials.polynomial
    |GreaterEqual of Polynomials.polynomial * Polynomials.polynomial
    |LessThan of Polynomials.polynomial * Polynomials.polynomial
    |LessEqual of Polynomials.polynomial * Polynomials.polynomial
    |Neq of Polynomials.polynomial * Polynomials.polynomial
    |Equal of Polynomials.polynomial * Polynomials.polynomial
    
let get_first_arg (comp : constraint_atom) =
    match comp with
    |GreaterThan(p1, p2) | GreaterEqual (p1, p2) | LessThan (p1, p2) | LessEqual (p1, p2) | Neq (p1, p2) | Equal (p1, p2)-> p1

let get_second_arg (comp : constraint_atom) =
    match comp with
    |GreaterThan(p1, p2) | GreaterEqual (p1, p2) | LessThan (p1, p2) | LessEqual (p1, p2) | Neq (p1, p2) | Equal (p1, p2)-> p2
    
let mk_gt (poly1 : Polynomials.polynomial) (poly2 : Polynomials.polynomial) =
    GreaterThan(poly1, poly2)

let mk_ge (poly1 : Polynomials.polynomial) (poly2 : Polynomials.polynomial) =
    GreaterEqual(poly1, poly2)

let mk_lt (poly1 : Polynomials.polynomial) (poly2 : Polynomials.polynomial) =
    LessThan(poly1, poly2)

let mk_le (poly1 : Polynomials.polynomial) (poly2 : Polynomials.polynomial) =
    LessEqual(poly1, poly2)

let mk_eq (poly1 : Polynomials.polynomial) (poly2 : Polynomials.polynomial) =
    Equal(poly1, poly2)
    
let mk_neq (poly1 : Polynomials.polynomial) (poly2 : Polynomials.polynomial) =
    Neq(poly1, poly2)
    
let one = Polynomials.one

(* In this setting everything represents integer values. Hence strictness can be removed by adding/subtracting one*)

let remove_strictness (comp : constraint_atom) =
    match comp with
    | GreaterThan (p1, p2)-> GreaterEqual (p1, (Polynomials.add p2 one))
    | LessThan (p1, p2)-> LessEqual( p1, (Polynomials.subtract p2 one))
    | _ -> comp
    
let to_string (comp : constraint_atom) =
    match comp with
    |GreaterThan (p1, p2)-> String.concat " > " [Polynomials.to_string p1; Polynomials.to_string p2]
    |GreaterEqual (p1, p2)-> String.concat " >= " [Polynomials.to_string p1; Polynomials.to_string p2]
    |LessThan (p1, p2)-> String.concat " < " [Polynomials.to_string p1; Polynomials.to_string p2]
    |LessEqual (p1, p2)-> String.concat " <= " [Polynomials.to_string p1; Polynomials.to_string p2]
    |Neq (p1, p2)-> String.concat " != " [Polynomials.to_string p1; Polynomials.to_string p2]
    |Equal (p1, p2)-> String.concat " = " [Polynomials.to_string p1; Polynomials.to_string p2]
    
let to_z3 (ctx : Z3.context) (comp : constraint_atom) =
    match comp with
    |GreaterThan (p1, p2)-> Z3.Arithmetic.mk_gt ctx (Polynomials.to_z3 ctx p1) (Polynomials.to_z3 ctx p2)
    |GreaterEqual (p1, p2)-> Z3.Arithmetic.mk_ge ctx (Polynomials.to_z3 ctx p1) (Polynomials.to_z3 ctx p2)
    |LessThan (p1, p2)-> Z3.Arithmetic.mk_lt ctx (Polynomials.to_z3 ctx p1) (Polynomials.to_z3 ctx p2)
    |LessEqual (p1, p2)-> Z3.Arithmetic.mk_le ctx (Polynomials.to_z3 ctx p1) (Polynomials.to_z3 ctx p2)
    |Equal (p1, p2)-> Z3.Boolean.mk_eq ctx (Polynomials.to_z3 ctx p1) (Polynomials.to_z3 ctx p2)
    |Neq (p1, p2)-> Z3.Boolean.mk_not ctx (Z3.Boolean.mk_eq ctx (Polynomials.to_z3 ctx p1) (Polynomials.to_z3 ctx p2))
    
let get_variables (comp : constraint_atom) =
    Tools.remove_dup (List.append (Polynomials.get_variables (get_first_arg comp)) (Polynomials.get_variables (get_second_arg comp)))
    
let rename_vars (varmapping : string VarMap.t) (comp : constraint_atom) =
    match comp with
    |GreaterThan (p1, p2)-> GreaterThan ((Polynomials.rename_vars varmapping p1), (Polynomials.rename_vars varmapping p2))
    |GreaterEqual (p1, p2)-> GreaterEqual ((Polynomials.rename_vars varmapping p1), (Polynomials.rename_vars varmapping p2))
    |LessThan (p1, p2)-> LessThan ((Polynomials.rename_vars varmapping p1), (Polynomials.rename_vars varmapping p2))
    |LessEqual (p1, p2)-> LessEqual ((Polynomials.rename_vars varmapping p1), (Polynomials.rename_vars varmapping p2))
    |Neq (p1, p2)-> Neq ((Polynomials.rename_vars varmapping p1), (Polynomials.rename_vars varmapping p2))
    |Equal (p1, p2)-> Equal ((Polynomials.rename_vars varmapping p1), (Polynomials.rename_vars varmapping p2))

let instantiate_with_big_int (varmapping : Big_int.big_int VarMap.t) (comp : constraint_atom) =
    match comp with
    |GreaterThan (p1, p2)-> (Big_int.gt_big_int (Polynomials.instantiate_with_big_int varmapping p1) (Polynomials.instantiate_with_big_int varmapping p2))
    |GreaterEqual (p1, p2)-> (Big_int.ge_big_int (Polynomials.instantiate_with_big_int varmapping p1) (Polynomials.instantiate_with_big_int varmapping p2))
    |LessThan (p1, p2)-> (Big_int.lt_big_int (Polynomials.instantiate_with_big_int varmapping p1) (Polynomials.instantiate_with_big_int varmapping p2))
    |LessEqual (p1, p2)-> (Big_int.le_big_int (Polynomials.instantiate_with_big_int varmapping p1) (Polynomials.instantiate_with_big_int varmapping p2))
    |Neq (p1, p2)-> not(Big_int.eq_big_int (Polynomials.instantiate_with_big_int varmapping p1) (Polynomials.instantiate_with_big_int varmapping p2))
    |Equal (p1, p2)-> (Big_int.eq_big_int (Polynomials.instantiate_with_big_int varmapping p1) (Polynomials.instantiate_with_big_int varmapping p2))