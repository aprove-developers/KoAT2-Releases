open! OurBase
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
  | Hypergeometric (bigN, k, n) when OurInt.(bigN = zero) -> RationalPolynomial.of_int 0
  | Hypergeometric (bigN, k, n) ->
      RationalPolynomial.(of_intpoly k * of_intpoly n * of_constant OurRational.(one / of_ourint bigN))
  | Uniform (a, b) ->
      RationalPolynomial.mul
        (RationalPolynomial.of_constant @@ OurRational.of_float 0.5)
        (RationalPolynomial.add (RationalPolynomial.of_intpoly a) (RationalPolynomial.of_intpoly b))


let rec moment_poly d i =
  if i = 0 then
    RationalPolynomial.of_int 1
  else if i = 1 then
    exp_value_poly d
  else
    match d with
    | Binomial (n, p) ->
        (* ∑_{j=0}^i (p-1)^j binom(n,j) ∑_{k=0}^j (-1)^k binom(j,k) (n-k)^i  [https://doi.org/10.1137/070700024, Theorem 4.1] *)
        let q = OurRational.(one - p) in
        RationalPolynomial.sum
        @@ Sequence.map
             OurInt.(range zero (of_int i))
             ~f:(fun j ->
               (* c = (-q)^j *)
               let c = RationalPolynomial.(pow (of_constant OurRational.(-q)) (OurInt.to_int j)) in
               (* c2 = binomial(n,j) *)
               let c2 = Math.binomial_poly n j in
               let inner_sum j =
                 RationalPolynomial.sum
                 @@ Sequence.map
                      OurInt.(range zero j)
                      ~f:(fun k ->
                        (* sgn = (-1)^k *)
                        let sgn =
                          if OurInt.(is_even k) then
                            RationalPolynomial.one
                          else
                            RationalPolynomial.(-one)
                        in
                        (* c3 = binomial(j,k) *)
                        let c3 = RationalPolynomial.of_intconstant @@ OurInt.binomial j k in
                        (* c4 = (n-k)^i *)
                        let c4 = RationalPolynomial.(pow (of_intpoly n - of_intconstant k) i) in
                        RationalPolynomial.(sgn * c3 * c4))
               in

               RationalPolynomial.(c * c2 * inner_sum j))
    | Geometric a ->
        RationalPolynomial.of_constant
        @@ OurRational.(a / (one - a) * Math.negative_polylog (OurInt.of_int i) (one - a))
        (* p/(1-p) * Li_{-n}(1-p) *)
    | Hypergeometric (bigN, k, n) ->
        if Polynomial.is_zero n || Polynomial.is_zero k || OurInt.is_zero bigN then
          RationalPolynomial.of_int 0
        else if Polynomial.(equal n (of_constant bigN)) then
          RationalPolynomial.(pow (of_intpoly k) i)
        else if Polynomial.(equal k (of_constant bigN)) then
          RationalPolynomial.(pow (of_intpoly n) i)
          (* TODO n and k are polynomials; what if one is a polynomial that can have the value 0 *)
        else
          (* 𝔼[X^i] = n*k/bigN * 𝔼[(Y+1)^(i-1)]  where Y is hypergeometric distributed with (bigN-1, k-1, n-1) *)
          (*        = n*k/bigN * ∑_{j=0}^(i-1) binomial(i-1,j) * 1^(i-1-j) * 𝔼(Y^j) *)
          (*        = n*k/bigN * ∑_{j=0}^(i-1) binomial(i-1,j) * 𝔼(Y^j) *)
          let nk_bigN =
            RationalPolynomial.(of_intpoly k * of_intpoly n * of_constant OurRational.(one / of_ourint bigN))
          in
          let sum_part =
            OurInt.(range zero OurInt.(of_int i - one))
            |> Sequence.map ~f:(fun j ->
                   let bin_coeff = RationalPolynomial.of_intconstant OurInt.(binomial (of_int i - one) j) in
                   let rec_res =
                     moment_poly
                       (Hypergeometric (OurInt.(bigN - one), Polynomial.(k - one), Polynomial.(n - one)))
                       (OurInt.to_int j)
                   in
                   RationalPolynomial.(bin_coeff * rec_res))
            |> RationalPolynomial.sum
          in
          RationalPolynomial.(nk_bigN * sum_part)
    | Uniform (a, b) ->
        (* Derived from 𝔼(X^n) = (B_{n+1} (b + 1) - B_{n+1} (a))/ ((n+1) (b - a + 1)) where B_n is Bernoulli polynomial *)
        (* 𝔼(X^n) = 1/(n+1) * ∑_{k=1}^{n+1} ((b+1)^k-a^k)/((b+1)-a) * bernoulli(n-k+1) * binomial(n+1,k) *)
        let rec multiplier k =
          if OurInt.(equal k one) then
            (* ((b+1)-a)/((b+1)-a) = 1 *)
            RationalPolynomial.one
          else if OurInt.(equal k (one + one)) then
            (* ((b+1)^2-a^2)/((b+1)-a) = (b+1)+a *)
            RationalPolynomial.of_intpoly Polynomial.(a + b + one)
          else
            (* (b+1)^k-a^k) = (b+1) * ( (b+1)^{k-1}-a^{k-1})         + a^{k-1} ((b+1)-a)  *)
            (*              = (b+1) * (multiplier(k-1) * ((b+1)-a))  + a^{k-1} ((b+1)-a)  *)
            (*              = ((b+1)-a)) * ((b+1) * (multiplier(k-1) + a^{k-1}) *)
            RationalPolynomial.(
              pow (of_intpoly a) OurInt.(to_int (k - one))
              + (of_intpoly Polynomial.(b + one) * multiplier OurInt.(k - one)))
        in
        RationalPolynomial.mult_with_const OurRational.(div one (of_int i + one))
        @@ RationalPolynomial.sum
        @@ Sequence.map
             ~f:(fun k ->
               RationalPolynomial.(
                 multiplier k
                 * of_constant (Math.bernoulli OurInt.(of_int i - k + one))
                 * of_intconstant OurInt.(binomial (of_int i + one) k)))
             OurInt.(range one (of_int i + one))


open Bounds

let exp_value_abs_bound = function
  | Uniform (a, b) -> RationalBound.(of_constant (OurRational.of_float 0.5) * (of_intpoly a + of_intpoly b))
  | Binomial (n, p) -> RationalBound.of_poly @@ exp_value_poly (Binomial (n, p))
  | Geometric a -> RationalBound.of_poly @@ exp_value_poly (Geometric a)
  | Hypergeometric (bigN, k, n) -> RationalBound.of_poly @@ exp_value_poly (Hypergeometric (bigN, k, n))


let moment_abs_bound d i =
  if Int.equal i 1 then
    exp_value_abs_bound d
  else
    match d with
    | Uniform (a, b) ->
        if i mod 2 = 0 then
          RationalBound.of_poly @@ moment_poly (Uniform (a, b)) i
        else
          failwith @@ Int.to_string i ^ ". moment of absolute uniform distribution not yet implemented."
    | Binomial (n, p) -> RationalBound.of_poly @@ moment_poly (Binomial (n, p)) i
    | Geometric a -> RationalBound.of_poly @@ moment_poly (Geometric a) i
    | Hypergeometric (bigN, k, n) -> RationalBound.of_poly @@ moment_poly (Hypergeometric (bigN, k, n)) i
