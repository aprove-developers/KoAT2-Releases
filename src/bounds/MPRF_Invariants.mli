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

val template_table_is_empty : bool

val template_table_clear : unit

val compute_invariant_templates : VarSet.t -> Location.t list -> unit

val store_inv : Valuation.t -> Transition.t -> unit

val store_inv_set : Valuation.t -> ProgramTypes.TransitionSet.t -> unit

val non_increasing_constraint : int -> measure -> Transition.t -> (Location.t -> RealParameterPolynomial.t) list -> RealFormula.t

val non_increasing_constraints : int -> measure -> TransitionSet.t ->  (Location.t -> RealParameterPolynomial.t) list -> RealFormula.t
  
val decreasing_constraint : int -> measure -> Transition.t -> (Location.t -> RealParameterPolynomial.t) list -> RealFormula.t

val initiation_constraint : int -> measure -> Transition.t -> (Location.t -> RealParameterPolynomial.t) list -> RealFormula.t

val disability_constraint : int -> measure -> Transition.t -> (Location.t -> RealParameterPolynomial.t) list -> RealFormula.t

val consecution_constraint : int -> measure -> Transition.t -> (Location.t -> RealParameterPolynomial.t) list -> RealFormula.t

val consecution_constraints : int -> measure -> TransitionSet.t -> (Location.t -> RealParameterPolynomial.t) list -> RealFormula.t

val clear_cache : unit