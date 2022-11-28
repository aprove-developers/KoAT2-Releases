(** Provides default module to handle programs. *)
open Batteries
open Polynomials
open Constraints
(** Provides default module to handle programs. *)

(** KoAT does not support recursion yet *)
exception RecursionNotSupported

module ProgramOver(L: ProgramTypes.Location) : sig
  include ProgramTypes.Program
    with type location = L.t
     and type location_set = Set.Make(L).t
     and type transition_set = Transition.TransitionSetOver(L).t
     and type transition_graph = TransitionGraph.TransitionGraphOver(L).t
end

include module type of ProgramOver(Location)

(** Returns a string representing the program that can be dumped to a KoAT input file. *)
val to_file : t -> string -> unit

(** Creates a program from a list of transitions and a (start) location. *)
(**  A list of k transitions makes up a Com_k transition *)
(**  Since KoAT currently does not support recursion we try to eliminate it. *)
(**  If this is not possible we throw a RecursionNotSupportedException *)
val from_com_transitions : Transition.t list list -> Location.t -> t

(** TODO doc *)
val rename : t -> t

(** Creates a file (if it does not already exist) and writes the program into it. *)
val to_file : t -> string -> unit
