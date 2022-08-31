open Batteries
open Polynomials
open ProgramTypes
open Atoms

type t

val vars: t -> VarSet.t 

val vars_as_list: t -> VarSet.elt list 

val poly_as_list: t -> Polynomials.RationalPolynomial.t list

(** [vars polys inv_polys] returns an automorphism, but does not check whether it truely is a mathematical automorphism*)
val of_poly_list: VarSet.elt list -> Polynomials.RationalPolynomial.t list -> Polynomials.RationalPolynomial.t list -> t

val identity_aut : t 
(** compose_rational_polynomials [[x;y]] [[x^2;y^3]] [(x <- x+y)]  returns [x <- x^2+y^3]. 
    tail recursive*)
val compose_rational_polynomials : Var.t list -> Polynomials.RationalPolynomial.t list -> Polynomials.RationalPolynomial.t -> Polynomials.RationalPolynomial.t

(** compose_int_polynomials [[x;y]] [[x^2;y^3]] [(x <- x+y)]  returns [x <- x^2+y^3]. 
    tail recursive*)
val compose_int_polynomials: Var.t list -> Polynomials.Polynomial.t list -> Polynomials.Polynomial.t -> Polynomials.Polynomial.t
(*  *)

val apply_poly_transformation:  Var.t list -> Polynomials.RationalPolynomial.t list -> Polynomials.RationalPolynomial.t list -> Polynomials.RationalPolynomial.t list

val apply: Polynomials.RationalPolynomial.t list -> t -> Polynomials.RationalPolynomial.t list

val apply_inv: Polynomials.RationalPolynomial.t list -> t -> Polynomials.RationalPolynomial.t list