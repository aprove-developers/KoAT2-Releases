open! Koat2.OurBase
open Koat2
open OUnit2
open Helper
open ProbabilityDistribution

(* TODO: Rework tests so that the “expected” moments are computed by instantiating the parameters with some numbers and then “manually” computing 𝔼(X^n) = ∑P(X = i)i^n *)

let parameters_for_uniform min max =
  let open ListMonad in
  let* a = List.range min max in
  let* b = List.range min max in
  guard (b >= a) >> pure (a, b)


let parameters_for_binomial minN maxN =
  let open ListMonad in
  let* p_denom = List.range 1 10 in
  let* n = List.range minN maxN in
  pure (OurInt.of_int n, OurRational.of_ints 1 p_denom)


let parameters_for_hypergeo minN maxN =
  let open ListMonad in
  let* bigN = List.range minN maxN in
  let* k = List.range 0 maxN in
  let* n = List.range 1 maxN in
  guard (bigN >= k && bigN >= n) >> pure (OurInt.of_int bigN, OurInt.of_int k, OurInt.of_int n)


let binominal_distr_N p = Binomial (Polynomials.Polynomial.of_var (Var.of_string "bigN"), p)
let binominal_distr1 = Binomial (Polynomials.Polynomial.of_int 6, OurRational.of_ints 3 5)
let binominal_distr2 = Binomial (Polynomials.Polynomial.of_int 30, OurRational.of_ints 1 5)
let geometric_distr1 = Geometric (OurRational.of_ints 2 5)
let geometric_distr2 = Geometric (OurRational.of_ints 1 10)

let hypergeometric_distr_k_n bigN =
  Hypergeometric
    ( bigN,
      Polynomials.Polynomial.of_var (Var.of_string "k"),
      Polynomials.Polynomial.of_var (Var.of_string "n") )


let hypergeometric_distr1 =
  Hypergeometric (OurInt.of_int 15, Polynomials.Polynomial.of_int 3, Polynomials.Polynomial.of_int 7)


let hypergeometric_distr2 =
  Hypergeometric (OurInt.of_int 100, Polynomials.Polynomial.of_int 10, Polynomials.Polynomial.of_int 60)


let uniform_distr_x_y =
  Uniform
    (Polynomials.Polynomial.of_var (Var.of_string "x"), Polynomials.Polynomial.of_var (Var.of_string "y"))


let uniform_distr1 = Uniform (Polynomials.Polynomial.of_int 10, Polynomials.Polynomial.of_int 20)
let uniform_distr2 = Uniform (Polynomials.Polynomial.of_int 0, Polynomials.Polynomial.of_int 100)

let tests =
  let make_moment_test (distr, moment1, moment2, moment3) =
    let calc_moment1 = ProbabilityDistribution.moment_poly distr 1 in
    let calc_moment2 = ProbabilityDistribution.moment_poly distr 2 in
    let calc_moment3 = ProbabilityDistribution.moment_poly distr 3 in
    let calc_moment1_OurRational = Polynomials.RationalPolynomial.get_constant calc_moment1 in
    let calc_moment2_OurRational = Polynomials.RationalPolynomial.get_constant calc_moment2 in
    let calc_moment3_OurRational = Polynomials.RationalPolynomial.get_constant calc_moment3 in
    "testing constant moments of distribution \"" ^ ProbabilityDistribution.to_string distr ^ "\""
    >::: [
           ( "first moment" >:: fun _ ->
             assert_equal moment1 calc_moment1_OurRational ~cmp:OurRational.equal
               ~printer:OurRational.to_string );
           ( "second moment" >:: fun _ ->
             assert_equal moment2 calc_moment2_OurRational ~cmp:OurRational.equal
               ~printer:OurRational.to_string );
           ( "third moment" >:: fun _ ->
             assert_equal moment3 calc_moment3_OurRational ~cmp:OurRational.equal
               ~printer:OurRational.to_string );
         ]
  in

  let make_moment_test_hypergeo_with_variables i (bigN, k, n) =
    let nth_moment_with_variable = ProbabilityDistribution.moment_poly (hypergeometric_distr_k_n bigN) i in
    let nth_moment_without_variable =
      moment_poly
        (Hypergeometric (bigN, Polynomials.Polynomial.of_constant k, Polynomials.Polynomial.of_constant n))
        i
    in
    let valuation_k_n v =
      if Var.equal v Var.(of_string "k") then
        OurRational.of_ourint k
      else
        OurRational.of_ourint n
    in
    let nth_moment_with_variable_value =
      Polynomials.RationalPolynomial.eval_f nth_moment_with_variable valuation_k_n
    in
    let nth_moment_without_variable_value =
      Polynomials.RationalPolynomial.get_constant nth_moment_without_variable
    in
    "test_Hypergeometric(" ^ OurInt.to_string bigN ^ "," ^ OurInt.to_string k ^ "," ^ OurInt.to_string n
    ^ ")^" ^ Int.to_string i
    >:: fun _ -> assert_equal_OurRational nth_moment_with_variable_value nth_moment_without_variable_value
  in

  let make_moment_test_binomial_with_variables i (bigN, p) =
    let nth_moment_with_variable n = ProbabilityDistribution.moment_poly (binominal_distr_N p) n in
    let nth_moment_without_variable n =
      moment_poly (Binomial (Polynomials.Polynomial.of_constant bigN, p)) n
    in
    let valuation_bigN v = OurRational.of_ourint bigN in
    let nth_moment_with_variable_value n =
      Polynomials.RationalPolynomial.eval_f (nth_moment_with_variable n) valuation_bigN
    in
    let nth_moment_without_variable_value n =
      Polynomials.RationalPolynomial.get_constant (nth_moment_without_variable n)
    in
    "test_Binomial(" ^ OurInt.to_string bigN ^ "," ^ OurRational.to_string p ^ ")" >:: fun _ ->
    assert_equal_OurRational (nth_moment_with_variable_value i) (nth_moment_without_variable_value i)
  in

  let make_moment_test_uniform_with_variables i (x, y) =
    let nth_moment_with_variable n = ProbabilityDistribution.moment_poly uniform_distr_x_y n in
    let nth_moment_without_variable n =
      moment_poly (Uniform (Polynomials.Polynomial.of_int x, Polynomials.Polynomial.of_int y)) n
    in
    let valuation_x_y v =
      if Var.equal v Var.(of_string "x") then
        OurRational.of_int x
      else
        OurRational.of_int y
    in
    let nth_moment_with_variable_value n =
      Polynomials.RationalPolynomial.eval_f (nth_moment_with_variable n) valuation_x_y
    in
    let nth_moment_without_variable_value n =
      Polynomials.RationalPolynomial.get_constant (nth_moment_without_variable n)
    in
    "test_Uniform(" ^ Int.to_string x ^ "," ^ Int.to_string y ^ ")" >:: fun _ ->
    assert_equal_OurRational (nth_moment_with_variable_value i) (nth_moment_without_variable_value i)
  in

  "Moments tests"
  >::: [
         "constant Moments"
         >::: [
                "Binomial"
                >::: List.map ~f:make_moment_test
                       [
                         ( binominal_distr1,
                           OurRational.of_ints 18 5,
                           OurRational.of_ints 72 5,
                           OurRational.of_ints 1548 25 );
                         ( binominal_distr2,
                           OurRational.of_ints 6 1,
                           OurRational.of_ints 204 5,
                           OurRational.of_ints 7632 25 );
                       ];
                "Geometric"
                >::: List.map ~f:make_moment_test
                       [
                         ( geometric_distr1,
                           OurRational.of_ints 5 2,
                           OurRational.of_ints 10 1,
                           OurRational.of_ints 235 4 );
                         ( geometric_distr2,
                           OurRational.of_ints 10 1,
                           OurRational.of_ints 190 1,
                           OurRational.of_ints 5410 1 );
                       ];
                "Hypergeometric"
                >::: List.map ~f:make_moment_test
                       [
                         ( hypergeometric_distr1,
                           OurRational.of_ints 7 5,
                           OurRational.of_ints 13 5,
                           OurRational.of_ints 71 13 );
                         ( hypergeometric_distr2,
                           OurRational.of_ints 6 1,
                           OurRational.of_ints 420 11,
                           OurRational.of_ints 137400 539 );
                       ];
                "Uniform"
                >::: List.map ~f:make_moment_test
                       [
                         ( uniform_distr1,
                           OurRational.of_ints 15 1,
                           OurRational.of_ints 235 1,
                           OurRational.of_ints 3825 1 );
                         ( uniform_distr2,
                           OurRational.of_ints 50 1,
                           OurRational.of_ints 3350 1,
                           OurRational.of_ints 252500 1 );
                       ];
              ];
         "moments with variables"
         >::: [
                "Uniform"
                >::: List.map (parameters_for_uniform (-10) 10) ~f:(make_moment_test_uniform_with_variables 3);
                "Binomial"
                >::: List.map (parameters_for_binomial 0 10) ~f:(make_moment_test_binomial_with_variables 3);
                "Hypergeometric"
                >::: List.map (parameters_for_hypergeo 0 10) ~f:(make_moment_test_hypergeo_with_variables 3);
              ];
       ]
