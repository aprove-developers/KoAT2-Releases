(** Implementation of local size-bounds. *)
open Batteries
open BoundsInst
open Formulas
open Polynomials
open ProgramTypes


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

(** Converts the templated bound to a string. *)
val to_string : t -> string

(** Takes a function that returns sizebounds for each variable and a local sizebound.
    Returns a bound representing the local sizebound with each variable substituted in a way that the bound is valid. *)
val as_substituted_bound : (Var.t -> Bound.t) -> t -> Bound.t

(** Converts the templated bound to an actual (finite) bound. *)
val as_bound : t -> Bound.t

val option_lsb_as_bound : t option -> Bound.t

(** Tries to find a templated bound of any of the defined templates. The first vars corresponds to
 * the variabels that may occur in the lsb *)
val find_bound : VarSet.t -> Var.t -> Formula.t -> int -> t option

(** Returns a local sizebound of the specified kind for the variable of the transition.
    A local sizebound is expressed in relation to the values directly before executing the transition. *)
val sizebound_local : Program.t -> Transition.t -> Var.t -> t Option.t

val sizebound_local_rv : Program.t -> (Transition.t * Var.t) -> t Option.t

(** If for all result variables of the given kind a local sizebound is defined, this function returns a local sizebound function.
    Otherwise it returns None. *)
val sizebound_local_scc : Program.t ->  (Transition.t * Var.t) list -> ((Transition.t * Var.t) -> t) Option.t

(** Resets all cached data.
    Useful for testing in the same OCaml instance. *)
val reset : unit -> unit

(** Resets all cached data used in the cfr computation.
    Useful for testing in the same OCaml instance. *)
val reset_cfr : unit -> unit

val switch_cache : unit -> unit

(** Enables cfr. *)
val enable_cfr : unit -> unit
