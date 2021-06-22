open Batteries
open Formulas
open Constraints
open Atoms
open Polynomials
open ProgramTypes

module Valuation :
sig
  type t = Valuation.Make(OurFloat).t
  type var = Var.t
  type value = OurFloat.t
  val from : (var * value) list -> t
  val from_native : (string * int) list -> t
  val zero : var list -> t
  val eval : var -> t -> value
  val eval_opt : var -> t -> value Batteries.Option.t
  val is_defined : t -> var -> bool
  val vars : t -> var list
  val bindings : t -> (var * value) Batteries.Enum.t
  val to_string : t -> string
end

type measure = [ `Cost | `Time ] [@@deriving show, eq]

type invariants_cache

val new_cache: unit -> invariants_cache

val compute_invariant_templates : invariants_cache -> VarSet.t -> Location.t list -> unit

val store_inv : invariants_cache -> Valuation.t -> Transition.t -> unit

val store_inv_set : invariants_cache -> Valuation.t -> ProgramTypes.TransitionSet.t -> unit

val non_increasing_constraint : invariants_cache -> int -> measure -> Transition.t -> (Location.t -> RealParameterPolynomial.t) list -> RealFormula.t

val non_increasing_constraints : invariants_cache -> int -> measure -> TransitionSet.t ->  (Location.t -> RealParameterPolynomial.t) list -> RealFormula.t

val decreasing_constraint : invariants_cache -> int -> measure -> Transition.t -> (Location.t -> RealParameterPolynomial.t) list -> RealFormula.t

val initiation_constraint : invariants_cache -> int -> measure -> Transition.t -> (Location.t -> RealParameterPolynomial.t) list -> RealFormula.t

val disability_constraint : invariants_cache -> int -> measure -> Transition.t -> (Location.t -> RealParameterPolynomial.t) list -> RealFormula.t

val consecution_constraint : invariants_cache -> int -> measure -> Transition.t -> (Location.t -> RealParameterPolynomial.t) list -> RealFormula.t

val consecution_constraints : invariants_cache ->  int -> measure -> TransitionSet.t -> (Location.t -> RealParameterPolynomial.t) list -> RealFormula.t
