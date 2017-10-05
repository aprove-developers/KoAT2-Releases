open Batteries
open Constraints
   
module FormulaOver(C : ConstraintTypes.Constraint) =
  struct

    module C = C
    
    type value = C.value
    type polynomial = C.polynomial
    type constr = C.t
    type atom = C.atom

    type t = C.t list

    let mk constr =
      [constr]
           
    let fold ~const ~var ~neg ~plus ~times ~pow ~le ~correct ~conj ~wrong ~disj =
      List.fold_left (fun c constr -> disj c (C.fold ~const ~var ~neg ~plus ~times ~pow ~le ~correct ~conj constr)) wrong
      
    let disj constraints =
      constraints

    let lift atom =
      mk (C.lift atom)
    
    let mk_true =
      mk C.mk_true

    let mk_false =
      []

    let mk_eq poly1 poly2 =
      mk (C.Infix.(poly1 = poly2))

    let mk_gt p1 p2 = mk (C.mk_gt p1 p2)
    let mk_ge p1 p2 = mk (C.mk_ge p1 p2)
    let mk_lt p1 p2 = mk (C.mk_lt p1 p2)
    let mk_le p1 p2 = mk (C.mk_le p1 p2)
    
    let mk_and formula1 formula2 =
      List.cartesian_product formula1 formula2
      |> List.map (uncurry C.mk_and)
      
    let mk_or =
      List.append

    let neg =
      fold ~const:C.A.P.from_constant
           ~var:C.A.P.from_var
           ~neg:C.A.P.neg
           ~plus:C.A.P.add
           ~times:C.A.P.mul
           ~pow:C.A.P.pow
           ~le:mk_gt
           ~correct:mk_false
           ~conj:mk_or
           ~wrong:mk_true
           ~disj:mk_and

    let implies formula1 formula2 =
      mk_or (neg formula1) formula2

    module Infix = struct
      let (=) = mk_eq
      let (>) = mk_gt
      let (>=) = mk_ge 
      let (<) = mk_lt 
      let (<=) = mk_le
      let (&&) = mk_and
      let (||) = mk_or
    end

    let all =
      List.fold_left mk_and mk_true

    let any =
      List.flatten
      
    (* a <= max{b1,...,bn}   <=>   a<=b1 || ... || a<=bn *)
    let le_than_any poly list =
         list
      |> List.map (fun b -> Infix.(poly <= b))
      |> any

    let le_than_all poly list =
         list
      |> List.map (fun b -> Infix.(poly <= b))
      |> all
      
    let vars formula =
         formula
      |> List.map (C.vars)
      |> List.fold_left VarSet.union VarSet.empty
        
    let to_string constr =
      String.concat " || " (List.map C.to_string constr)
        
    let rename formula varmapping =
      List.map (fun constr -> C.rename constr varmapping) formula

    let turn =
      List.map C.turn
      
  end

module Formula =
  struct
    include FormulaOver(Constraint)
  end

module ParameterFormula =
  struct
    include FormulaOver(ParameterConstraint)
  end
