open Batteries
open Polynomials
module Guard = Constraints.Constraint
open BoundsInst

type t = Uniform of Polynomial.t * Polynomial.t
       | Geometric of OurFloat.t [@@deriving eq,ord]

let deterministic_upper_bound dist =
  match dist with
    | Uniform (a,b) -> Bound.of_poly b
    | Geometric _   -> Bound.infinity

let deterministic_lower_bound dist =
  match dist with
    | Uniform (a,b) -> Bound.of_poly a
    | Geometric _   -> Bound.one

let deterministic_upper_polynomial dist =
  match dist with
    | Uniform (a,b) -> Some b
    | Geometric _   -> None

let deterministic_lower_polynomial dist =
  match dist with
    | Uniform (a,b) -> Some b
    | Geometric _   -> Some Polynomial.one

let expected_value dist =
  match dist with
    | Uniform (a,b) -> RealPolynomial.((of_constant (Num.of_float 0.5)) * ((of_intpoly a) + (of_intpoly b)))
    | Geometric a   -> RealPolynomial.of_constant( OurFloat.(div one a) )

let vars dist =
  match dist with
    | Uniform (a,b) ->
        VarSet.union (Polynomial.vars a) (Polynomial.vars b)
    | Geometric _   -> VarSet.empty

let to_string d =
  match d with
    | Uniform (a,b) -> "Uniform " ^ (Polynomial.to_string a) ^ " " ^ (Polynomial.to_string b)
    | Geometric p   -> "Geometric " ^ (OurFloat.to_string p)

let rename rename_map dist =
  match dist with
    | Uniform (a,b) -> Uniform ((Polynomial.rename rename_map a), (Polynomial.rename rename_map b))
    | Geometric p   -> Geometric p

let guard dist var =
  match dist with
    | Uniform (a,b) -> Guard.Infix.(((Polynomial.of_var var) <= b) &&
                                    ((Polynomial.of_var var) >= a)     )
    | Geometric p   -> Guard.Infix.((Polynomial.of_var var) > Polynomial.zero)

let upper_det_const d =
  match d with
    | Uniform (a,b) -> if Polynomial.is_const b then Some (Polynomial.get_constant b)
                       else None
    | Geometric p   -> None
let lower_det_const d =
  match d with
    | Uniform (a,b) -> if Polynomial.is_const b then Some (Polynomial.get_constant a)
                       else None
    | Geometric p   -> Some OurInt.one

let substitute sub d =
  match d with
    | Uniform (a,b) -> Uniform (Polynomial.substitute_f sub a,Polynomial.substitute_f sub b)
    | Geometric p   -> Geometric p

