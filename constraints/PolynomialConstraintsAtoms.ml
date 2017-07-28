open Batteries
open PolyTypes
open ConstraintTypes

module MakePolynomialConstraintsAtom(P : Polynomial) =
(*Polynomial Constraints of the form p1<p2, p1<=p2, etc. Conjunctions of these constraints form the real constraints*)
struct
    module Polynomial_ = P

    type comparator = GT | GE | LT | LE | NEQ | EQ        
                                              
    type t = P.t * comparator * P.t
             
    let get_comparator = function 
      | (_,comparator,_) -> comparator
                 
    let get_first_arg = function
      | (fst,_,_) -> fst

    let get_second_arg = function
      | (_,_,snd) -> snd

    let mk comp poly1 poly2 =
      (poly1, comp, poly2)

    let mk_gt = mk GT
    let mk_ge = mk GE
    let mk_lt = mk LT
    let mk_le = mk LE
    let mk_eq = mk EQ
    let mk_neq = mk NEQ

    let is comp atom =
      (get_comparator atom) == comp

    let is_gt = is GT
    let is_ge = is GE
    let is_lt = is LT
    let is_le = is LE
    let is_eq = is EQ
    let is_neq = is NEQ
            
    let is_same_constr (atom1 : t) (atom2 : t) =
      get_comparator atom1 == get_comparator atom2
        
    let is_inverted_comparator (comp1 : comparator) (comp2 : comparator) =
      match (comp1, comp2) with
      | (GT, LT) -> true
      | (GE, LE) -> true
      | (LT, GT) -> true
      | (LE, GE) -> true
      | (EQ, EQ) -> true
      | (NEQ, NEQ) -> true
      | (_,_) -> false

    let is_inverted_constr (atom1 : t) (atom2 : t) =
      is_inverted_comparator (get_comparator atom1) (get_comparator atom2)
        
    let simplify = function
      | (p1, comp, p2)-> (P.simplify p1, comp, P.simplify p2)
                
    let (==) (atom1 : t) (atom2 : t) =
      match (atom1, atom2) with
      | ((p1, comp1, q1), (p2, comp2, q2)) ->
         comp1 == comp2 && P.(==) p1 p2 && P.(==) q1 q2
            
    let is_redundant (atom1 : t) (atom2 : t) =
      match (atom1, atom2) with
      | ((p1, comp1, q1), (p2, comp2, q2)) ->
         (is_inverted_comparator comp1 comp2 && P.(==) p1 q2 && P.(==) q1 p2) || atom1 == atom2 
        
    (* In this setting everything represents integer values. Hence strictness can be removed by adding/subtracting one*)

    let remove_strictness (constr : t) =
      match constr with
      | (p1, GT, p2)-> mk GE p1 (P.add p2 P.one)
      | (p1, LT, p2)-> mk LE p1 (P.sub p2 P.one)
      | _ -> constr

    let comparator_to_string = function
      | GT -> ">"
      | GE -> ">="
      | LT -> "<"
      | LE -> "<="
      | EQ -> "="
      | NEQ -> "!="

    let to_string = function
      | (p1, comp, p2) -> String.concat " " [P.to_string p1; comparator_to_string comp; P.to_string p2]
        
    let get_variables = function
      | (p1, comp, p2) -> List.unique (List.append (P.vars p1) (P.vars p2))
        
    let rename_vars (constr : t) (varmapping : P.RenameMap_.t) =
      match constr with
      | (p1, comp, p2) -> (P.rename varmapping p1, comp, P.rename varmapping p2)

    let comparator_to_function = function
      | GT -> (P.Value.Compare.(>))
      | GE -> (P.Value.Compare.(>=))
      | LT -> (P.Value.Compare.(<))
      | LE -> (P.Value.Compare.(<=))
      | EQ -> (P.Value.Compare.(=))
      | NEQ -> (P.Value.Compare.(<>))

    (* TODO It's maybe possible to compare polynomials without full evaluation *)
    (* However, there are probably more expensive operations *)
    let eval_bool (constr : t) (varmapping : P.Valuation_.t) =
      match constr with
      | (p1, comp, p2) -> (comparator_to_function comp) (P.eval p1 varmapping) (P.eval p2 varmapping)
        
end
