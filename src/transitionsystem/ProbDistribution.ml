open Polynomials
module Guard = Constraints.Constraint
open BoundsInst

type t = Uniform of Polynomial.t * Polynomial.t [@@deriving eq,ord]

let deterministic_upper_bound dist =
  match dist with
    | Uniform (a,b) -> Bound.of_poly b

let deterministic_lower_bound dist = 
  match dist with
    | Uniform (a,b) -> Bound.of_poly a

let deterministic_upper_polynomial dist = 
  match dist with 
    | Uniform (a,b) -> Some b

let expected_value dist = 
  match dist with
    | Uniform (a,b) -> RealPolynomial.((of_constant 0.5) * ((of_intpoly a) + (of_intpoly b)))

let vars dist = 
  match dist with 
    | Uniform (a,b) -> 
        VarSet.union (Polynomial.vars a) (Polynomial.vars b) 

let to_string d = 
  match d with
    | Uniform (a,b) -> "Uniform " ^ (Polynomial.to_string a) ^ " " ^ (Polynomial.to_string b)

let rename rename_map dist = 
  match dist with
    | Uniform (a,b) -> Uniform ((Polynomial.rename rename_map a), (Polynomial.rename rename_map b))

let guard dist var =
  match dist with 
    | Uniform (a,b) -> Guard.Infix.(((Polynomial.of_var var) <= b) &&
                                    ((Polynomial.of_var var) >= a)     )

let upper_det_const d = 
  match d with 
    | Uniform (a,b) -> if Polynomial.is_const b then Some (Polynomial.get_constant b)
                       else None
let lower_det_const d = 
  match d with 
    | Uniform (a,b) -> if Polynomial.is_const b then Some (Polynomial.get_constant a)
                       else None

let substitute sub d = 
  match d with
    | Uniform (a,b) -> Uniform (Polynomial.substitute_f sub a,Polynomial.substitute_f sub b)

