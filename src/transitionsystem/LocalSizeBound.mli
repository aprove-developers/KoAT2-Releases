(** Implementation of local size-bounds. *)
open Batteries
open BoundsInst
open Formulas
open Polynomials
open ProgramTypes
open ProgramModules

module Make(PM: ProgramTypes.ClassicalProgramModules): sig
  (** LocalSizeBounds are of the form factor * (constant + sum [x1;...;xn]) *)
  type t

  (** Constructs a local size bound with the variables specified as string list*)
  val mk : ?s:int -> ?c:int -> string list -> t

  (** Returns if the templated bounds represent the same bound. *)
  val equal : t -> t -> bool

  (** Returns the factor of the local sizebound. Raises unbounded, if the local size bound is unbounded*)
  val factor : t -> int

  (** Returns the constant of the local sizebound. Raises unbounded, if the local size bound is unbounded*)
  val constant : t -> int

  (** Returns a set of of variables which affect the local sizebound *)
  val vars : t -> VarSet.t

  (** Is the LocalSizeBound constant, i.e. does it contain no variables? *)
  val is_constant : t -> bool

  (** Converts the templated bound to a string. *)
  val to_string : t -> string

  (** Converts the templated bound to an actual (finite) bound. *)
  val as_bound : t -> Bound.t

  val option_lsb_as_bound : t option -> Bound.t

  (** Tries to find a templated bound of any of the defined templates. The first vars corresponds to
  * the variabels that may occur in the lsb.
  * The bool Lazy.t value indiciates if this lsb is of the equality type *)
  val find_bound : VarSet.t -> Var.t -> Formula.t -> int -> (t * bool Lazy.t) option

  (** Returns a local sizebound of the specified kind for the variable of the transition.
      A local sizebound is expressed in relation to the values directly before executing the transition. *)
  val sizebound_local : PM.Program.t -> PM.Transition.t -> Var.t -> t Option.t

  val sizebound_local_rv : PM.Program.t -> (PM.Transition.t * Var.t) -> t Option.t

  (** If for all result variables of the given kind a local sizebound is defined, this function returns a local sizebound function.
      Otherwise it returns None. *)
  val sizebound_local_scc : PM.Program.t ->  (PM.Transition.t * Var.t) list -> ((PM.Transition.t * Var.t) -> t * bool) Option.t

  (** Resets all cached data.
      Useful for testing in the same OCaml instance. *)
  val reset : unit -> unit

  (** Resets all cached data used in the cfr computation.
      Useful for testing in the same OCaml instance. *)
  val reset_cfr : unit -> unit

  val switch_cache : unit -> unit

  (** Enables cfr. *)
  val enable_cfr : unit -> unit
end

include module type of Make(ProgramModules)
