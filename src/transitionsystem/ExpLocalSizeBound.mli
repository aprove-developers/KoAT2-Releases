open ProgramTypes
open RVGTypes
open Polynomials
open BoundsInst

type kind = [ `Lower | `Upper ]

module RV : sig include module type of Make_RV (RVTransitions.TransitionForExpectedSize) end

val vars : Program.t -> kind -> RV.t -> VarSet.t

(* Computes an expected local size bound *)
val elsb : Program.t -> kind -> RV.t -> RealBound.t

(* Checks whether a given (possibly multivariate) bound is concave using SMT-Solving *)
val bound_is_concave : RealBound.t -> bool
(* Checks whether a given (possibly multivariate) bound is convexe using SMT-Solving *)
val bound_is_convexe : RealBound.t -> bool

(* Given a transition remove the probabilistic update, i.e. if an update element
 * is a distribution try to bound its value *)
val det_update : kind -> Transition.t * Var.t -> RealPolynomial.t option

(* If the guards encode x = a for a variable x and a constant a then substitute x by a*)
val simplify_poly_with_guard : TransitionLabel.Guard.t -> RealPolynomial.t -> RealPolynomial.t
