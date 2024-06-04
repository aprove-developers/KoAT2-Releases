open! OurBase
open Polynomials

(** [binomial_poly n k] computes the binomial coefficient choose(n,k) where n is a polynomial*)
let binomial_poly n k =
  let k_fac = OurInt.fac (OurInt.to_int k) in
  let n_k =
    Sequence.fold ~f:Polynomial.( * ) ~init:Polynomial.one
    @@ Sequence.map ~f:(fun j -> Polynomial.(n - Polynomial.of_constant j)) OurInt.(range zero (k - one))
  in
  RationalPolynomial.mult_with_const OurRational.(one / of_ourint k_fac) @@ RationalPolynomial.of_intpoly n_k


(** [negative_polylog n z] computes the negative polylogarithm Li_{-n} (z) = \sum_{i=1}^\infty i^n/z^i for |z| < 1 *)
let negative_polylog n z =
  (* Li_{-n}(z) = sum_{k=0}^n (-z/(1-z))^{k+1} * sum_{j=0}^k (-1)^{j+1} binomial(k,j) (j+1)^n *)
  OurRational.sum
  @@ Sequence.map
       ~f:(fun k ->
         let c = OurRational.pow OurRational.(-z / (one - z)) (OurInt.to_int k + 1) in
         let inner_sum k =
           OurRational.sum
           @@ Sequence.map
                ~f:(fun j ->
                  let sgn =
                    if OurInt.is_even j then
                      OurRational.minus_one
                    else
                      OurRational.one
                  in
                  OurRational.(
                    sgn * of_ourint (OurInt.binomial k j) * pow (of_ourint j + one) (OurInt.to_int n)))
                OurInt.(range zero k)
         in
         OurRational.(c * inner_sum k))
       OurInt.(range zero n)


(** [bernoulli n] computes the n-th bernoulli number *)
let bernoulli n =
  if n == OurInt.zero then
    OurRational.one
  else if OurInt.(equal n one) then
    OurRational.of_ints (-1) 2
  else
    OurRational.sum
    @@ Sequence.map
         ~f:(fun k ->
           let inner_sum k =
             OurRational.sum
             @@ Sequence.map
                  ~f:(fun j ->
                    let sgn =
                      if OurInt.(is_even j) then
                        OurRational.one
                      else
                        OurRational.minus_one
                    in
                    OurRational.(of_ourint (OurInt.binomial k j) * pow (of_ourint j) OurInt.(to_int n) * sgn))
                  OurInt.(range zero k)
           in
           OurRational.(one / (of_ourint k + one) * inner_sum k))
         OurInt.(range zero n)
