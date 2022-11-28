(** Provides default module to handle programs. *)
open Batteries
open Polynomials
open Constraints
(** Provides default module to handle programs. *)

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

(** TODO doc *)
val rename : t -> t

(** KoAT does not support recursion yet *)
exception RecursionNotSupported
