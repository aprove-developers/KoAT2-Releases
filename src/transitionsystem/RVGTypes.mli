open! OurBase
(** Provides all modules related to result variable graphs. *)

(** Module handling result variables. *)
module MakeRV
    (TL : ProgramTypes.TransitionLabel)
    (T : ProgramTypes.Transition with type transition_label = TL.t) : sig
  (** Module handling result variables. *)

  include
    ProgramTypes.RV with type transition = T.t and type transition_comparator_witness = T.comparator_witness
end

module RV : module type of MakeRV (TransitionLabel_) (Transition_)

(** Module handling result variable graphs. *)
module MakeRVG (PM : ProgramTypes.ClassicalProgramModules) : sig
  include module type of
      Graph.Persistent.Digraph.ConcreteBidirectional (MakeRV (PM.TransitionLabel) (PM.Transition))
  (** Module handling result variable graphs, i.e., a digraph where the nodes are result variables. *)

  val rvs_to_id_string : MakeRV(PM.TransitionLabel)(PM.Transition).t list -> string
  (** Returns a string which is created by calling [to_id_string] on every result variable. *)

  val pre :
    t -> MakeRV(PM.TransitionLabel)(PM.Transition).t -> MakeRV(PM.TransitionLabel)(PM.Transition).t List.t
  (** Returns the predecessors of a result variable in the result variable graph. *)

  type scc = MakeRV(PM.TransitionLabel)(PM.Transition).t list

  val rvg : (MakeRV(PM.TransitionLabel)(PM.Transition).t -> VarSet.t Option.t) -> PM.Program.t -> t
  (** Compute the result variable graph.
      The first argument computes the variables in the corresponding lsb or None if no such (finite) lsb exists *)

  val rvg_with_sccs :
    (MakeRV(PM.TransitionLabel)(PM.Transition).t -> VarSet.t Option.t) -> PM.Program.t -> t * scc list Lazy.t
  (** Compute the result variable graph and lazily compute the list of all SCCs
      The first argument computes the variables in the corresponding lsb or None if no such (finite) lsb exists *)
end
