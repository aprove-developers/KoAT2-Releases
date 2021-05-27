(** Implementation of local size-bounds. *)
open Batteries
open BoundsInst
open Formulas
open Polynomials
open ProgramTypes

(* TODO doc *)

(* Concept:
   Incoming part:
   For UPPER bounds the MAXIMUM of all incoming variables leading to the SCC or a constant bound anywhere in the SCC.
   For LOWER bounds the MINIMUM of all incoming variables leading to the SCC or a constant bound anywhere in the SCC.
   Constant adds:
   For UPPER bounds the sum over all transitions of the SCC, where we differ two cases:
   If there exists a POSITIVE added constant for any of its result variables, then we multiply the HIGHEST constant with the upper runtime bound of the transition.
   If all added constants are negative for all of its result variables, then we multiply the HIGHEST constant (next toward zero) with the lower runtime bound of the transition.
   For LOWER bounds the sum over all transitions of the SCC, where we also differ two cases:
   If there exists a NEGATIVE added constant for any of its result variables, then we multiply the LOWEST constant with the upper runtime bound of the transition.
   If all added constants are POSITIVE for all of its result variables, then we multiply the LOWEST constant (next toward zero) with the lower runtime bound of the transition.
   Scaled sums: upcoming
 *)

(** A templated bound is a bound of a certain templated form.
    The different templates are not disjunctive.
    The upcoming template set always includes the previous one. *)
(* TODO We can not use the sum of all variables, if the value of a variable might be negative and not actually used in the transition.  *)
(** Always smaller or equal to a scaling factor multiplied with the sum of all prevariables and a constant. Examples: x'=x+y , x'=2*(x+y+z)
    s * (e + sum [x1;...;xn]) *)
type t

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
