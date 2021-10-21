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

    let mk_uneq p1 p2 =
      mk_or (mk_lt p1 p2) (mk_gt p1 p2)

    let constraints formula =
      formula

    let fold ~subject ~le ~lt ~correct ~conj ~wrong ~disj =
      List.fold_left (fun c constr -> disj c (C.fold ~subject ~le ~lt ~correct ~conj constr)) wrong

    let map_polynomial f =
      fold ~subject:f ~le:mk_le ~lt:mk_lt ~correct:mk_true ~conj:mk_and ~wrong:mk_false ~disj:mk_or

    let neg =
      fold ~subject:identity
           ~le:mk_gt
           ~lt:mk_gt
           ~correct:mk_false
           ~conj:mk_or
           ~wrong:mk_true
           ~disj:mk_and

    let implies formula1 formula2 =
      mk_or (neg formula1) formula2

    let equivalent formula1 formula2 =
      mk_and (implies formula1 formula2) (implies formula2 formula1)

    module Infix = struct
      let (=) = mk_eq
      let (>) = mk_gt
      let (>=) = mk_ge
      let (<) = mk_lt
      let (<=) = mk_le
      let (&&) = mk_and
      let (||) = mk_or
      let (=>) = implies
      let (<=>) = equivalent
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
      String.concat " || " (List.map C.to_string  constr)

    let to_string_pretty = function
      | [] -> FormattedString.Empty
      | x::xs -> 
        (C.to_string ~pretty:true x)::(List.map (((^) " ∨ ") % C.to_string ~pretty:true) xs) 
        |> List.map FormattedString.mk_str_line
        |> List.reduce FormattedString.(<>)

    let rename formula varmapping =
      List.map (fun constr -> C.rename constr varmapping) formula

    let turn =
      List.map C.turn

    let is_linear = List.for_all C.is_linear

  end

module Formula =
  struct
    include FormulaOver(Constraint)

    let max_of_occurring_constants constraints =
      constraints
      |> List.map Constraint.max_of_occurring_constants
      |> List.fold_left OurInt.add OurInt.one

    let simplify = 
      (List.unique ~eq:Constraint.equal) % (List.map Constraint.simplify) 

  end

module ParameterFormula =
  struct
    include FormulaOver(ParameterConstraint)
  end

module RealFormula =
  struct
    include FormulaOver(RealConstraint)

    let of_intformula =
      Formula.fold
        ~subject:(Polynomials.RealPolynomial.of_intpoly)
        ~le:mk_le
        ~lt:mk_lt
        ~correct:mk_true
        ~conj:mk_and
        ~wrong:mk_false
        ~disj:mk_or

    let max_of_occurring_constants constraints =
      constraints
      |> List.map RealConstraint.max_of_occurring_constants
      |> List.fold_left OurFloat.mul OurFloat.one

  end

module RealParameterFormula =
  struct
    include FormulaOver(RealParameterConstraint)
  end
