open ProgramTypes
open RVGTypes
open Polynomials
open BoundsInst

module RV : sig include module type of Make_RV (RVTransitions.TransitionForExpectedSize) end

val vars : Program.t -> RV.t -> VarSet.t

(** Computes an expected local size bound *)
val elsb : Program.t -> RV.t -> RealBound.t

(** Checks whether a given (possibly multivariate) bound is concave using SMT-Solving *)
val bound_is_concave : RealBound.t -> bool

(** Checks whether a given (possibly multivariate) bound is convexe using SMT-Solving *)
val bound_is_convexe : RealBound.t -> bool

(** If the guards encode x = a for a variable x and a constant a then substitute x by a*)
val simplify_poly_with_guard : TransitionLabel.Guard.t -> RealPolynomial.t -> RealPolynomial.t
