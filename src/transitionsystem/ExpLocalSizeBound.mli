open ProgramTypes
open RVGTypes
open Polynomials
open BoundsInst

type elsb_cache
val new_cache: unit -> elsb_cache

type t

val elsb : t -> RealBound.t
val elsb_plus_var : t -> RealBound.t
val reduced_elsb : t -> RealBound.t

module RV : sig include module type of Make_RV (RVTransitions.TransitionForExpectedSize) end
module NPRV : sig include module type of RVGTypes.RVG.RV end

(* This computes the variables occuring in an expected update. Note that
   var program rv = var(elsb(rv) + v) where v is the variable of rv.
   Hence when considering the vars of a rv we get the variables necessary to compute
   the (expected) next value (for example using the nontrivial sizebounds method)  *)
val vars : elsb_cache -> Program.t -> RV.t -> VarSet.t

(** Computes an expected local size bound. The first returned element is the expected change bound, whereas
    the second returned element is an expected local size bound*)
val compute_elsb : elsb_cache -> Program.t -> RV.t -> t

(** Checks whether a given (possibly multivariate) bound is concave using SMT-Solving *)
val bound_is_concave : elsb_cache -> RealBound.t -> bool

(** Checks whether a given (possibly multivariate) bound is convexe using SMT-Solving *)
val bound_is_convexe : elsb_cache -> RealBound.t -> bool
