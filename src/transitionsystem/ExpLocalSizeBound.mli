open ProgramTypes
open RVGTypes
open Polynomials
open BoundsInst

type elsb_cache
val new_cache: unit -> elsb_cache

module RV : sig include module type of Make_RV (RVTransitions.TransitionForExpectedSize) end
module NPRV : sig include module type of Make_RV (Transition) end

(* This computes the variables occuring in an expected update. Note that
   var program rv = var(elsb(rv) + v) where v is the variable of rv.
   Hence when considering the vars of a rv we get the variables necessary to compute
   the (expected) next value (for example using the nontrivial sizebounds method)  *)
val vars : elsb_cache -> Program.t -> RV.t -> VarSet.t

(** Computes an expected local size bound *)
val elsb : elsb_cache -> Program.t -> RV.t -> RealBound.t

(** Checks whether a given (possibly multivariate) bound is concave using SMT-Solving *)
val bound_is_concave : elsb_cache -> RealBound.t -> bool

(** Checks whether a given (possibly multivariate) bound is convexe using SMT-Solving *)
val bound_is_convexe : elsb_cache -> RealBound.t -> bool

(** If the guards encode x = a for a variable x and a constant a then substitute x by a*)
val simplify_poly_with_guard : TransitionLabel.Guard.t -> RealPolynomial.t -> RealPolynomial.t