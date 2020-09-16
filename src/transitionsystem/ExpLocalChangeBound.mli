open ProgramTypes
open RVGTypes
open Polynomials
open BoundsInst


module RV : sig include module type of Make_RV (RVTransitions.TransitionForExpectedSize) end
module NPRV : sig include module type of RVGTypes.RVG.RV end

type elcb_cache
val new_cache: unit -> elcb_cache

(* Computes an expected local change bound *)
val elcb : elcb_cache -> RV.t -> RealBound.t

(* Computes an expected local size bound by inferring it from an expected local change bound by
 * adding the considered variables absolute value to the change bound *)
val elcb_plus_var : elcb_cache -> RV.t -> RealBound.t

(* This computes the variables occuring in an expected update. Note that
   var program rv = var(elcb(rv) + v) where v is the variable of rv.
   Hence when considering the vars of a rv we get the variables necessary to compute
   the (expected) next value (for example using the nontrivial sizebounds method)  *)
val vars : elcb_cache -> RV.t -> VarSet.t

(** Checks whether a given (possibly multivariate) bound is concave using SMT-Solving *)
val bound_is_concave : elcb_cache -> RealBound.t -> bool

(** Checks whether a given (possibly multivariate) bound is convex using SMT-Solving *)
val bound_is_convex : elcb_cache -> RealBound.t -> bool
