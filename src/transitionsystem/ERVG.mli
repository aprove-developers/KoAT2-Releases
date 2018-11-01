open Batteries
open RVGTypes
open ProgramTypes

module RV : sig include module type of Make_RV (RVTransitions.TransitionForExpectedSize) end
include module type of Graph.Persistent.Digraph.ConcreteBidirectional(struct
                           include RV
                           let equal = same
                           let compare = compare_same
                         end)

val rvs_to_id_string : RV.t list -> string

val pre : t -> RV.t -> RV.t Enum.t

(** Returns all the entry points of the SCC.
    Those are all result variables that are in the RVG, but not in the SCC and lead to any result variable in the RVG. *)
val entry_points : t -> RV.t list -> RV.t Enum.t

(** Returns all transitions that are used in the SCC of the RVG. *)
val transitions : RV.t list -> RVTransitions.TransitionForExpectedSize.t Enum.t

val rvg : Program.t -> t
