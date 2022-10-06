open Batteries
open Polynomials
open ProgramTypes
open Atoms
open Formulas

module Endomorphism  :
sig
    type t
    val to_string: t -> string

    val vars: t -> VarSet.t

    val vars_as_list: t -> Var.t list

    val identity_end : t

    val poly_list: t -> Polynomials.ParameterPolynomial.t list

    val poly_map: t -> (Polynomials.ParameterPolynomial.t Map.Make(Var).t)

    val inv_poly_map: t -> (Polynomials.ParameterPolynomial.t Map.Make(Var).t)

    val inv_poly_list: t -> Polynomials.ParameterPolynomial.t list

    val of_poly_list: Var.t list -> Polynomials.ParameterPolynomial.t list -> t

    val of_degree: Var.t list -> int -> t

    val apply:  t -> Polynomials.ParameterPolynomial.t -> Polynomials.ParameterPolynomial.t

    val enumerate_all_polys_degree: int -> Var.t list -> Polynomials.Polynomial.t list

    val degree: t -> int

    val formula_to_check_invertibility: t -> Formulas.Formula.t

    val formula_to_check_twn: VarSet.elt list -> t -> Polynomials.PolynomialOver(OurInt).t Batteries.Map.Make(Var).t -> Formulas.Formula.t

end
module Automorphism :
sig
    type t

    val to_string: t -> string

    val vars: t -> VarSet.t

    val poly_map: t -> (Polynomials.RationalPolynomial.t Map.Make(Var).t)

    val inv_poly_map: t -> (Polynomials.Polynomial.t Map.Make(Var).t)

    val inv_rational_poly_map: t -> (Polynomials.RationalPolynomial.t Map.Make(Var).t)

    val vars_as_list: t -> Var.t list

    val inv_poly_list: t -> Polynomials.Polynomial.t list

    val inv_rational_poly_list: t -> Polynomials.RationalPolynomial.t list

    val poly_list: t -> Polynomials.RationalPolynomial.t list

    (** [vars polys inv_polys] returns an automorphism, but does not check whether it truely is a mathematical automorphism*)
    val of_poly_list: Var.t list -> Polynomials.RationalPolynomial.t list -> Polynomials.Polynomial.t list -> t

    val of_endomorphism: Endomorphism.t -> Polynomials.Polynomial.valuation -> t

    val identity_aut : t

    (** applies the inverse automorphism to the bound*)
    val transform_bound: t -> BoundsInst.Bound.t -> BoundsInst.Bound.t

    (**eta_inv(update(eta(x))) *)
    val transform_update: t -> Polynomials.RationalPolynomial.t Map.Make(Var).t ->Polynomials.Polynomial.t list option

    val transform_guard: t -> TransitionLabel.Guard.t -> TransitionLabel.Guard.t

end