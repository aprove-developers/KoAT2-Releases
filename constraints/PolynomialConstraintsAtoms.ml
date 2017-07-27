open Batteries
open PolyTypes
open ConstraintTypes

module MakePolynomialConstraintsAtom(Var : ID) (Value : Number.Numeric) =
(*Polynomial Constraints of the form p1<p2, p1<=p2, etc. Conjunctions of these constraints form the real constraints*)
struct
    module Polynomial_ = Polynomials.MakePolynomial(Var)(Value)
    
    type polynomial = Polynomial_.t
        
    type t = 
        |GreaterThan of polynomial * polynomial
        |GreaterEqual of polynomial * polynomial
        |LessThan of polynomial * polynomial
        |LessEqual of polynomial * polynomial
        |Neq of polynomial * polynomial
        |Equal of polynomial * polynomial

    (* TODO Make t = polynomial * comparator * polynomial for comfortability *)
    type comparator = GT | GE | LT | LE | NEQ | EQ
                
    module Var = Var
    module Value = Value

    let get_comparator = function 
      | GreaterThan(_,_) -> GT
      | GreaterEqual(_,_) -> GE
      | LessThan(_,_) -> LT
      | LessEqual(_,_) -> LE
      | Neq(_,_) -> NEQ
      | Equal(_,_) -> EQ
                 
    let get_first_arg (comp : t) =
        match comp with
        |GreaterThan(p1, p2) | GreaterEqual (p1, p2) | LessThan (p1, p2) | LessEqual (p1, p2) | Neq (p1, p2) | Equal (p1, p2)-> p1

    let get_second_arg (comp : t) =
        match comp with
        |GreaterThan(p1, p2) | GreaterEqual (p1, p2) | LessThan (p1, p2) | LessEqual (p1, p2) | Neq (p1, p2) | Equal (p1, p2)-> p2
        
    let mk_gt (poly1 : polynomial) (poly2 : polynomial) =
        GreaterThan(poly1, poly2)

    let mk_ge (poly1 : polynomial) (poly2 : polynomial) =
        GreaterEqual(poly1, poly2)

    let mk_lt (poly1 : polynomial) (poly2 : polynomial) =
        LessThan(poly1, poly2)

    let mk_le (poly1 : polynomial) (poly2 : polynomial) =
        LessEqual(poly1, poly2)

    let mk_eq (poly1 : polynomial) (poly2 : polynomial) =
        Equal(poly1, poly2)
        
    let mk_neq (poly1 : polynomial) (poly2 : polynomial) =
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
        
    let is_inverted_constr (atom1 : t) (atom2 : t) =
        match (atom1, atom2) with
        |(GreaterThan (_,_), LessThan(_,_)) -> true
        |(GreaterEqual (_,_), LessEqual(_,_)) -> true
        |(LessThan (_,_), GreaterThan(_,_)) -> true
        |(LessEqual (_,_), GreaterEqual(_,_)) -> true
        |(Equal (_,_), Equal (_,_)) -> true
        |(Neq (_,_), Neq (_,_)) -> true
        |(_,_) -> false
        
    let simplify (atom : t) =
        let poly_simplify = Polynomial_.simplify in
            match atom with
                |GreaterThan (p1, p2)-> mk_gt (poly_simplify p1) (poly_simplify p2)
                |GreaterEqual (p1, p2)-> mk_ge (poly_simplify p1) (poly_simplify p2)
                |LessThan (p1, p2)-> mk_lt (poly_simplify p1) (poly_simplify p2)
                |LessEqual (p1, p2)-> mk_le (poly_simplify p1) (poly_simplify p2)
                |Neq (p1, p2)-> mk_neq (poly_simplify p1) (poly_simplify p2)
                |Equal (p1, p2)-> mk_eq (poly_simplify p1) (poly_simplify p2)
                
    let (==) (atom1 : t) (atom2 : t) =
        let poly_equal = Polynomial_.(==) in
            (is_same_constr atom1 atom2) && (poly_equal (get_first_arg atom1) (get_first_arg atom2)) && (poly_equal (get_second_arg atom1) (get_second_arg atom2))
            
    let is_redundant (atom1 : t) (atom2 : t) =
        let poly_equal = Polynomial_.(==) in
            ((is_inverted_constr atom1 atom2) && (poly_equal (get_first_arg atom1) (get_second_arg atom2)) && (poly_equal (get_second_arg atom1) (get_first_arg atom2))) || ( atom1 == atom2 )
        
    let one = Polynomial_.one

    (* In this setting everything represents integer values. Hence strictness can be removed by adding/subtracting one*)

    let remove_strictness (comp : t) =
        match comp with
        | GreaterThan (p1, p2)-> GreaterEqual (p1, (Polynomial_.add p2 one))
        | LessThan (p1, p2)-> LessEqual( p1, (Polynomial_.sub p2 one))
        | _ -> comp
        
    let to_string (comp : t) =
        match comp with
        |GreaterThan (p1, p2)-> String.concat " > " [Polynomial_.to_string p1; Polynomial_.to_string p2]
        |GreaterEqual (p1, p2)-> String.concat " >= " [Polynomial_.to_string p1; Polynomial_.to_string p2]
        |LessThan (p1, p2)-> String.concat " < " [Polynomial_.to_string p1; Polynomial_.to_string p2]
        |LessEqual (p1, p2)-> String.concat " <= " [Polynomial_.to_string p1; Polynomial_.to_string p2]
        |Neq (p1, p2)-> String.concat " != " [Polynomial_.to_string p1; Polynomial_.to_string p2]
        |Equal (p1, p2)-> String.concat " = " [Polynomial_.to_string p1; Polynomial_.to_string p2]
        
    let get_variables (comp : t) =
        List.unique (List.append (Polynomial_.vars (get_first_arg comp)) (Polynomial_.vars (get_second_arg comp)))
        
    let rename_vars (comp : t) (varmapping : Polynomial_.RenameMap_.t) =
        match comp with
        |GreaterThan (p1, p2)-> GreaterThan ((Polynomial_.rename varmapping p1), (Polynomial_.rename varmapping p2))
        |GreaterEqual (p1, p2)-> GreaterEqual ((Polynomial_.rename varmapping p1), (Polynomial_.rename varmapping p2))
        |LessThan (p1, p2)-> LessThan ((Polynomial_.rename varmapping p1), (Polynomial_.rename varmapping p2))
        |LessEqual (p1, p2)-> LessEqual ((Polynomial_.rename varmapping p1), (Polynomial_.rename varmapping p2))
        |Neq (p1, p2)-> Neq ((Polynomial_.rename varmapping p1), (Polynomial_.rename varmapping p2))
        |Equal (p1, p2)-> Equal ((Polynomial_.rename varmapping p1), (Polynomial_.rename varmapping p2))

    let eval_bool (comp : t) (varmapping : Polynomial_.Valuation_.t) =
        match comp with
        |GreaterThan (p1, p2)-> (Value.Compare.(>) (Polynomial_.eval p1 varmapping) (Polynomial_.eval p2 varmapping))
        |GreaterEqual (p1, p2)-> (Value.Compare.(>=) (Polynomial_.eval p1 varmapping) (Polynomial_.eval p2 varmapping))
        |LessThan (p1, p2)-> (Value.Compare.(<) (Polynomial_.eval p1 varmapping) (Polynomial_.eval p2 varmapping))
        |LessEqual (p1, p2)-> (Value.Compare.(<=) (Polynomial_.eval p1 varmapping) (Polynomial_.eval p2 varmapping))
        |Neq (p1, p2)-> (Value.Compare.(<>) (Polynomial_.eval p1 varmapping) (Polynomial_.eval p2 varmapping))
        |Equal (p1, p2)-> (Value.Compare.(=) (Polynomial_.eval p1 varmapping) (Polynomial_.eval p2 varmapping))
        
end
