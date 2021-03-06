open Batteries
open ProgramTypes

module Make_RV :
  functor (Trans :
                  sig
                    type t
                    val same: t -> t -> bool
                    val equivalent: t -> t -> bool
                    val compare_same: t -> t -> int
                    val compare_equivalent: t -> t -> int
                    val to_string: t -> string
                    val to_id_string: t -> string
                  end) ->
    sig
      type t = Trans.t * Var.t
      val same : t -> t -> bool
      val equivalent : t -> t -> bool
      val compare_same : t -> t -> int
      val compare_equivalent : t -> t -> int
      val to_id_string : t -> string
      val hash : t -> int
      val transition : t -> Trans.t
      val variable : t -> Var.t
    end

module RVG :
sig
  module RV : sig include module type of Make_RV (struct
                                                    include Transition
                                                    let to_string = to_string ~show_gtcost:false
                                                  end) end
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
  val transitions : RV.t list -> Transition.t Enum.t

  type rvg_cache
  val new_cache: unit -> rvg_cache
  val rvg : rvg_cache -> Program.pre_cache -> LocalSizeBound.lsb_cache -> [`Lower | `Upper] -> Program.t -> t

end
