open OurBase
open Polynomials

type t =
  | Binomial of Polynomial.t * OurFloat.t
  | Geometric of OurFloat.t
  | Hypergeometric of OurInt.t * Polynomial.t * Polynomial.t
  | Uniform of Polynomial.t * Polynomial.t
[@@deriving eq, ord]

let to_string ?(pretty = false) ?(to_file = false) =
  let poly_to_string =
    if pretty then
      Polynomial.to_string_pretty
    else if to_file then
      Polynomial.to_string_to_file
    else
      Polynomial.to_string
  in
  function
  | Binomial (n, p) -> "Binomial (" ^ poly_to_string n ^ ", " ^ OurFloat.to_string p ^ ")"
  | Geometric p -> "Geometric (" ^ OurFloat.to_string p ^ ")"
  | Hypergeometric (bigN, k, n) ->
      "Hypergeometric (" ^ OurInt.to_string bigN ^ ", " ^ Polynomial.to_string k ^ ", "
      ^ Polynomial.to_string n ^ ")"
  | Uniform (p1, p2) -> "UNIFORM(" ^ poly_to_string p1 ^ ", " ^ poly_to_string p2 ^ ")"


let rename m = function
  | Binomial (n, p) -> Binomial (Polynomial.rename m n, p)
  | Geometric p -> Geometric p
  | Hypergeometric (bigN, k, n) -> Hypergeometric (bigN, Polynomial.rename m k, Polynomial.rename m n)
  | Uniform (p1, p2) -> Uniform (Polynomial.rename m p1, Polynomial.rename m p2)


let admissibility_constraint =
  let zero = Polynomial.zero in
  function
  | Binomial (n, _) -> Guard.Infix.(zero <= n)
  | Hypergeometric (bigN, k, n) ->
      let bigN = Polynomial.of_constant bigN in
      Guard.Infix.(zero <= bigN && zero <= k && zero <= n && k <= bigN && n <= bigN)
  | Uniform (p1, p2) -> Guard.Infix.(p1 <= p2)
  | _ -> Guard.mk_true


let vars = function
  | Binomial (n, _) -> Polynomial.vars n
  | Geometric _ -> VarSet.empty
  | Hypergeometric (_, k, n) -> Set.union (Polynomial.vars k) (Polynomial.vars n)
  | Uniform (p1, p2) -> Set.union (Polynomial.vars p1) (Polynomial.vars p2)


let as_guard d v' =
  let poly_v' = Polynomial.of_var v' in
  let zero = Polynomial.zero in
  match d with
  | Binomial (n, p) ->
      if OurFloat.(equal p zero) then
        Guard.Infix.(poly_v' = zero)
      else if OurFloat.(equal p one) then
        Guard.Infix.(poly_v' = n)
      else
        Guard.Infix.(zero <= poly_v' && poly_v' <= n)
  | Geometric p -> Guard.Infix.(poly_v' > Polynomial.zero)
  | Hypergeometric (bigN, k, n) -> Guard.Infix.(zero <= poly_v' && poly_v' <= n)
  | Uniform (a, b) -> Guard.Infix.(a <= poly_v' && poly_v' <= b)


let exp_value_poly = function
  | Binomial (n, p) -> RealPolynomial.(of_intpoly n * of_constant p)
  | Geometric a -> RealPolynomial.of_constant OurFloat.(div (of_int 1) a)
  | Hypergeometric (bigN, k, n) ->
      RealPolynomial.(of_intpoly k * of_intpoly n * of_constant OurFloat.(one / of_ourint bigN))
  | Uniform (a, b) ->
      RealPolynomial.mul
        (RealPolynomial.of_constant @@ OurFloat.of_float 0.5)
        (RealPolynomial.add (RealPolynomial.of_intpoly a) (RealPolynomial.of_intpoly b))


(* TODO generalise to higher orders we might want to use polylogs for geo distribution *)
let moment_poly d i =
  if i = 1 then
    exp_value_poly d
  else
    match d with
    | Binomial (n, p) -> (
        match i with
        | 2 ->
            let n, p = (RealPolynomial.of_intpoly n, RealPolynomial.of_constant p) in
            RealPolynomial.((n * p * (one - p)) + (pow n 2 * pow p 2))
        | _ -> failwith @@ Int.to_string i ^ ". moment of binomial distribution not yet implemented.")
    | Geometric a -> (
        match i with
        | 2 -> RealPolynomial.of_constant @@ OurFloat.((of_int 2 - a) / pow a 2)
        | _ -> failwith @@ Int.to_string i ^ ". moment of geometric distribution not yet implemented.")
    | Hypergeometric (bigN, k, n) -> (
        match i with
        (* RealPolynomial.(of_intpoly k * of_intpoly n * of_constant OurFloat.(one/of_ourint bigN)) *)
        | 2 ->
            let expv = exp_value_poly d in
            let bigN, k, n =
              (OurFloat.of_ourint bigN, RealPolynomial.of_intpoly k, RealPolynomial.of_intpoly n)
            in
            RealPolynomial.(
              expv
              + expv
                * (of_constant OurFloat.(one / bigN) * (of_constant bigN - k))
                * (of_constant OurFloat.(one / (bigN - one)) * (of_constant bigN - n)))
        | _ -> failwith @@ Int.to_string i ^ ". moment of hypergeometric distribution not yet implemented.")
    | Uniform (a, b) -> (
        match i with
        | 2 ->
            (* Variance is given by ((b - a + 1)² - 1)/12 *)
            let one = RealPolynomial.of_constant OurFloat.one in
            let variance =
              RealPolynomial.mul
                (RealPolynomial.of_constant @@ OurFloat.div OurFloat.one (OurFloat.of_int 12))
                (RealPolynomial.sub
                   (RealPolynomial.pow
                      (RealPolynomial.add
                         (RealPolynomial.sub (RealPolynomial.of_intpoly b) (RealPolynomial.of_intpoly a))
                         one)
                      2)
                   one)
            in
            let exp_squared = RealPolynomial.pow (exp_value_poly d) 2 in
            RealPolynomial.add variance exp_squared
        (* TODO *)
        | _ -> failwith @@ Int.to_string i ^ ". moment of uniform distribution not yet implemented.")


open Bounds

let exp_value_abs_bound = function
  | Uniform (a, b) -> RealBound.(of_constant (OurFloat.of_float 0.5) * (of_intpoly a + of_intpoly b))
  | Binomial (n, p) -> RealBound.of_poly @@ exp_value_poly (Binomial (n, p))
  | Geometric a -> RealBound.of_poly @@ exp_value_poly (Geometric a)
  | Hypergeometric (bigN, k, n) -> RealBound.of_poly @@ exp_value_poly (Hypergeometric (bigN, k, n))


let moment_abs_bound d i =
  if Int.equal i 1 then
    exp_value_abs_bound d
  else
    match d with
    | Uniform (a, b) ->
        if i mod 2 = 0 then
          RealBound.of_poly @@ moment_poly (Uniform (a, b)) i
        else
          failwith @@ Int.to_string i ^ ". moment of absolute uniform distribution not yet implemented."
    | Binomial (n, p) -> RealBound.of_poly @@ moment_poly (Binomial (n, p)) i
    | Geometric a -> RealBound.of_poly @@ moment_poly (Geometric a) i
    | Hypergeometric (bigN, k, n) -> RealBound.of_poly @@ moment_poly (Hypergeometric (bigN, k, n)) i
