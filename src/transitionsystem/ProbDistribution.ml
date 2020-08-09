open Batteries
open Polynomials
module Guard = Constraints.Constraint
open BoundsInst

(* Note the sampled value always gets added onto the current variable value *)

type t = Binomial of Polynomial.t * OurFloat.t
       | Geometric of OurFloat.t
       | Hypergeometric of OurInt.t * Polynomial.t * Polynomial.t
       | Uniform of Polynomial.t * Polynomial.t [@@deriving eq,ord]

let deterministic_upper_polynomial dist =
  match dist with
    | Binomial (n,p)            -> if OurFloat.(equal zero p) then Some Polynomial.zero else Some n
    | Geometric      _          -> None
    | Hypergeometric (bigN,k,n) -> Some n
    | Uniform (a,b)             -> Some b

let deterministic_lower_polynomial dist =
  match dist with
    | Binomial (n,p)            -> if OurFloat.(equal one p) then Some n else Some Polynomial.zero
    | Geometric _               -> Some Polynomial.one
    | Hypergeometric (bigN,k,n) -> Some Polynomial.zero
    | Uniform (a,b)             -> Some a

let expected_value dist =
  match dist with
    | Binomial (n,p)            -> RealPolynomial.(of_intpoly n * of_constant p)
    | Geometric a               -> RealPolynomial.of_constant( OurFloat.(div (of_int 1) a) )
    | Hypergeometric (bigN,k,n) -> RealPolynomial.(of_intpoly k * of_intpoly n * of_constant OurFloat.(one/of_ourint bigN))
    | Uniform (a,b)             -> RealPolynomial.((of_constant (Num.of_float 0.5)) * ((of_intpoly a) + (of_intpoly b)))

let expected_value_abs dist =
  match dist with
    | Binomial (n,p)            -> expected_value (Binomial (n,p)) |> RealBound.of_poly
    | Geometric a               -> expected_value (Geometric a)    |> RealBound.of_poly
    | Hypergeometric (bigN,k,n) -> expected_value (Hypergeometric (bigN,k,n)) |> RealBound.of_poly
    | Uniform (a,b)             ->
        RealBound.((of_constant (Num.of_float 0.5)) *
          (abs (of_poly @@ RealPolynomial.of_intpoly a) + abs (of_poly @@ RealPolynomial.of_intpoly b)))

let vars dist =
  match dist with
    | Binomial (n,_)   -> Polynomial.vars n
    | Geometric _      -> VarSet.empty
    | Hypergeometric (bigN,k,n) -> VarSet.union (Polynomial.vars n) (Polynomial.vars k)
    | Uniform (a,b)    ->
        VarSet.union (Polynomial.vars a) (Polynomial.vars b)

let to_string d =
  match d with
    | Binomial (n,p)            -> "Binomial (" ^ (Polynomial.to_string n) ^ ", " ^ (OurFloat.to_string p) ^ ")"
    | Geometric p               -> "Geometric (" ^ (OurFloat.to_string p) ^ ")"
    | Hypergeometric (bigN,k,n) ->
        "Hypergeometric (" ^ (OurInt.to_string bigN) ^ ", " ^
        (Polynomial.to_string k) ^ ", " ^
        (Polynomial.to_string n) ^ ")"
    | Uniform (a,b)             -> "Uniform (" ^ (Polynomial.to_string a) ^ ", " ^ (Polynomial.to_string b) ^ ")"

let rename rename_map dist =
  match dist with
    | Binomial (n,p)            -> Binomial ((Polynomial.rename rename_map n), p)
    | Geometric p               -> Geometric p
    | Hypergeometric (bigN,k,n) -> Hypergeometric (bigN, Polynomial.rename rename_map k, Polynomial.rename rename_map n)
    | Uniform (a,b)             -> Uniform ((Polynomial.rename rename_map a), (Polynomial.rename rename_map b))

let guard dist v v' =
  match dist with
    | Binomial (n,p) ->
        if OurFloat.(equal p zero) then
          Guard.Infix.(Polynomial.of_var v' = Polynomial.of_var v)
        else
          if OurFloat.(equal p one) then
            Guard.Infix.(Polynomial.of_var v' = Polynomial.(n + of_var v))
          else
            Guard.Infix.(Polynomial.of_var v' <= Polynomial.(n+ of_var v) &&
                        Polynomial.of_var v' >= Polynomial.of_var v)
    | Geometric p   -> Guard.Infix.(Polynomial.of_var v' > Polynomial.of_var v)
    | Hypergeometric (bigN,k,n) ->
        Guard.Infix.(Polynomial.of_var v' <= Polynomial.(n + of_var v) &&
                     Polynomial.of_var v' >= Polynomial.of_var v)
    | Uniform (a,b) -> Guard.Infix.(Polynomial.of_var v' <= Polynomial.(b + of_var v) &&
                                    Polynomial.of_var v' >= Polynomial.(a + of_var v) )

let substitute sub d =
  match d with
    | Binomial (n,p)            -> Binomial (Polynomial.substitute_f sub n,p)
    | Geometric p               -> Geometric p
    | Hypergeometric (bigN,k,n) -> Hypergeometric (bigN, Polynomial.substitute_f sub k, Polynomial.substitute_f sub n)
    | Uniform (a,b)             -> Uniform (Polynomial.substitute_f sub a,Polynomial.substitute_f sub b)

