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

    val poly_map: t -> (Polynomials.ParameterPolynomial.t Map.Make(Var).t)

    val inv_poly_map: t -> (Polynomials.ParameterPolynomial.t Map.Make(Var).t)

    val poly_list: t -> Polynomials.ParameterPolynomial.t list

    val inv_poly_list: t -> Polynomials.ParameterPolynomial.t list

    val of_poly_list: Var.t list -> Polynomials.ParameterPolynomial.t list -> t

    val of_degree: Var.t list -> int -> t

    val identity_end : t 

    val apply:  t -> Polynomials.ParameterPolynomial.t -> Polynomials.ParameterPolynomial.t

    val enumerate_help: int -> int -> int list list 

    val enumerate_all_polys_degree: int -> Var.t list -> Polynomials.Polynomial.t list

    val degree_of_polylist: Polynomials.ParameterPolynomial.t list -> int
    (**returns the formula to check right invertibility for one variable, it is technically not p_r_i as in Termination of Polynomial Loops but its later mentioned transformed version *)
    val get_p_r_i: t -> 'a -> Var.t -> Formulas.Formula.t
    (**returns the formula to check right invertibility for one variable, it is technically not p_r_i as in Termination of Polynomial Loops but its later mentioned transformed version *)
    val get_p_l_i: t -> 'a -> Var.t -> Formulas.Formula.t
    
    val formula_to_check_invertibility: t -> Formulas.Formula.t

    val formula_to_check_twn: t -> Formulas.Formula.t

end
module Automorphism :
sig
    type t

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

    (** applies the inverse automorphism to the bound*)
    val transform_bound: t -> BoundsInst.Bound.t -> BoundsInst.Bound.t

    (**eta(update(eta_inv(x))) *)
    val transform_update: t -> Polynomials.RationalPolynomial.t Map.Make(Var).t ->Polynomials.Polynomial.t list
    
    val transform_guard: t -> TransitionLabel.Guard.t -> TransitionLabel.Guard.t

end