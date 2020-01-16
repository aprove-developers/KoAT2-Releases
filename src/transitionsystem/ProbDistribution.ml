open Batteries
open Polynomials
module Guard = Constraints.Constraint
open BoundsInst

(* Note the sampled value always gets added onto the current variable value *)

type t = Binomial of Polynomial.t * OurFloat.t
       | Geometric of OurFloat.t
       | Uniform of Polynomial.t * Polynomial.t [@@deriving eq,ord]

let deterministic_upper_polynomial dist =
  match dist with
    | Binomial (n,p) -> Some n
    | Geometric _    -> None
    | Uniform (a,b)  -> Some b

let deterministic_lower_polynomial dist =
  match dist with
    | Binomial _    -> Some Polynomial.zero
    | Geometric _   -> Some Polynomial.zero
    | Uniform (a,b) -> Some b

let expected_value dist =
  match dist with
    | Binomial (n,p) -> RealPolynomial.(of_intpoly n * of_constant p)
    | Geometric a    -> RealPolynomial.of_constant( OurFloat.(div (of_float 0.5) a) )
    | Uniform (a,b)  -> RealPolynomial.((of_constant (Num.of_float 0.5)) * ((of_intpoly a) + (of_intpoly b)))

let expected_value_abs dist =
  match dist with
    | Binomial (n,p) -> expected_value (Binomial (n,p)) |> RealBound.of_poly
    | Geometric a    -> expected_value (Geometric a)    |> RealBound.of_poly
    | Uniform (a,b)  ->
        RealBound.((of_constant (Num.of_float 0.5)) *
          (abs (of_poly @@ RealPolynomial.of_intpoly a) + abs (of_poly @@ RealPolynomial.of_intpoly b)))

let vars dist =
  match dist with
    | Binomial (n,_) -> Polynomial.vars n
    | Geometric _    -> VarSet.empty
    | Uniform (a,b)  ->
        VarSet.union (Polynomial.vars a) (Polynomial.vars b)

let to_string d =
  match d with
    | Binomial (n,p) -> "Binomial (" ^ (Polynomial.to_string n) ^ ", " ^ (OurFloat.to_string p) ^ ")"
    | Geometric p    -> "Geometric (" ^ (OurFloat.to_string p) ^ ")"
    | Uniform (a,b)  -> "Uniform (" ^ (Polynomial.to_string a) ^ ", " ^ (Polynomial.to_string b) ^ ")"

let rename rename_map dist =
  match dist with
    | Binomial (n,p) -> Binomial ((Polynomial.rename rename_map n), p)
    | Geometric p    -> Geometric p
    | Uniform (a,b)  -> Uniform ((Polynomial.rename rename_map a), (Polynomial.rename rename_map b))

let guard dist v v' =
  match dist with
    | Binomial (n,p) -> Guard.Infix.(Polynomial.of_var v' <= Polynomial.(n+ of_var v) &&
                                     Polynomial.of_var v' >= Polynomial.of_var v)
    | Geometric p   -> Guard.Infix.(Polynomial.of_var v' >= Polynomial.of_var v)
    | Uniform (a,b) -> Guard.Infix.(Polynomial.of_var v' <= Polynomial.(b + of_var v) &&
                                    Polynomial.of_var v' >= Polynomial.(a + of_var v) )

let substitute sub d =
  match d with
    | Binomial (n,p) -> Binomial (Polynomial.substitute_f sub n,p)
    | Geometric p   -> Geometric p
    | Uniform (a,b) -> Uniform (Polynomial.substitute_f sub a,Polynomial.substitute_f sub b)

