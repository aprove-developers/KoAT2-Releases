open OurBase
open Polynomials

type t =
  | Binomial of Polynomial.t * OurRational.t
  | Geometric of OurRational.t
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
  | Binomial (n, p) -> "Binomial (" ^ poly_to_string n ^ ", " ^ OurRational.to_string p ^ ")"
  | Geometric p -> "Geometric (" ^ OurRational.to_string p ^ ")"
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
      if OurRational.(equal p zero) then
        Guard.Infix.(poly_v' = zero)
      else if OurRational.(equal p one) then
        Guard.Infix.(poly_v' = n)
      else
        Guard.Infix.(zero <= poly_v' && poly_v' <= n)
  | Geometric p -> Guard.Infix.(poly_v' > Polynomial.zero)
  | Hypergeometric (bigN, k, n) -> Guard.Infix.(zero <= poly_v' && poly_v' <= n)
  | Uniform (a, b) -> Guard.Infix.(a <= poly_v' && poly_v' <= b)


let exp_value_poly = function
  | Binomial (n, p) -> RationalPolynomial.(of_intpoly n * of_constant p)
  | Geometric a -> RationalPolynomial.of_constant OurRational.(div (of_int 1) a)
  | Hypergeometric (bigN, k, n) ->
      RationalPolynomial.(of_intpoly k * of_intpoly n * of_constant OurRational.(one / of_ourint bigN))
  | Uniform (a, b) ->
      RationalPolynomial.mul
        (RationalPolynomial.of_constant @@ OurRational.of_float 0.5)
        (RationalPolynomial.add (RationalPolynomial.of_intpoly a) (RationalPolynomial.of_intpoly b))


(* TODO generalise to higher orders we might want to use polylogs for geo distribution *)
let moment_poly d i =
  if i = 1 then
    exp_value_poly d
  else
    match d with
    | Binomial (n, p) -> (
        match i with
        | 2 ->
            let n, p = (RationalPolynomial.of_intpoly n, RationalPolynomial.of_constant p) in
            RationalPolynomial.((n * p * (one - p)) + (pow n 2 * pow p 2))
        | _ -> failwith @@ Int.to_string i ^ ". moment of binomial distribution not yet implemented.")
    | Geometric a -> (
        match i with
        | 2 -> RationalPolynomial.of_constant @@ OurRational.((of_int 2 - a) / pow a 2)
        | _ -> failwith @@ Int.to_string i ^ ". moment of geometric distribution not yet implemented.")
    | Hypergeometric (bigN, k, n) -> (
        match i with
        (* RationalPolynomial.(of_intpoly k * of_intpoly n * of_constant OurRational.(one/of_ourint bigN)) *)
        | 2 ->
            let expv = exp_value_poly d in
            let bigN, k, n =
              (OurRational.of_ourint bigN, RationalPolynomial.of_intpoly k, RationalPolynomial.of_intpoly n)
            in
            RationalPolynomial.(
              expv
              + expv
                * (of_constant OurRational.(one / bigN) * (of_constant bigN - k))
                * (of_constant OurRational.(one / (bigN - one)) * (of_constant bigN - n)))
        | _ -> failwith @@ Int.to_string i ^ ". moment of hypergeometric distribution not yet implemented.")
    | Uniform (a, b) -> (
        match i with
        | 2 ->
            (* Variance is given by ((b - a + 1)² - 1)/12 *)
            let one = RationalPolynomial.of_constant OurRational.one in
            let variance =
              RationalPolynomial.mul
                (RationalPolynomial.of_constant @@ OurRational.div OurRational.one (OurRational.of_int 12))
                (RationalPolynomial.sub
                   (RationalPolynomial.pow
                      (RationalPolynomial.add
                         (RationalPolynomial.sub (RationalPolynomial.of_intpoly b)
                            (RationalPolynomial.of_intpoly a))
                         one)
                      2)
                   one)
            in
            let exp_squared = RationalPolynomial.pow (exp_value_poly d) 2 in
            RationalPolynomial.add variance exp_squared
        (* TODO *)
        | _ -> failwith @@ Int.to_string i ^ ". moment of uniform distribution not yet implemented.")


open Bounds

let exp_value_abs_bound = function
  | Uniform (a, b) -> RealBound.(of_constant (OurRational.of_float 0.5) * (of_intpoly a + of_intpoly b))
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
