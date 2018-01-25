open Batteries
open ProgramTypes

module RV :
sig
  type t = Transition.t * Var.t
  val same : t -> t -> bool
  val equivalent : t -> t -> bool
  val compare_same : t -> t -> int
  val compare_equivalent : t -> t -> int
  val to_id_string : t -> string
  val to_string : ([`Lower | `Upper] -> t -> Bound.t) -> [`Lower | `Upper] -> t -> string
  val hash : t -> int
  val transition : t -> Transition.t
  val variable : t -> Var.t
end
     
module RVG :
sig
  include module type of Graph.Persistent.Digraph.ConcreteBidirectional(struct
                             include RV
                             let equal = same
                             let compare = compare_same
                           end)

  val rvs_to_id_string : RV.t list -> string

  val rvs_to_string : ([`Lower | `Upper] -> RV.t -> Bound.t) -> RV.t list -> string

  val pre : t -> RV.t -> RV.t Enum.t

  (** Returns all the entry points of the SCC.
      Those are all result variables that are in the RVG, but not in the SCC and lead to any result variable in the RVG. *)
  val entry_points : t -> RV.t list -> RV.t Enum.t

  (** Returns all transitions that are used in the SCC of the RVG. *)
  val transitions : RV.t list -> Transition.t Enum.t
    
end
