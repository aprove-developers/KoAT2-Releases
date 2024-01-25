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

  type rv = MakeRV(PM.TransitionLabel)(PM.Transition).t
  type scc = rv list

  val rvs_to_id_string : rv list -> string
  (** Returns a string which is created by calling [to_id_string] on every result variable. *)

  val pre : t -> rv -> rv List.t
  (** Returns the predecessors of a result variable in the result variable graph. *)

  val rvg : (rv -> VarSet.t Option.t) -> PM.Program.t -> t
  (** Compute the result variable graph.
      The first argument computes the variables in the corresponding lsb or None if no such (finite) lsb exists *)

  val rvg_from_transitionset : (rv -> VarSet.t Option.t) -> PM.Program.t -> PM.TransitionSet.t -> t
  (** Similar to [rvg] but only considers the transition of the given [TransitionSet] and their outgoing transitions *)

  val rvg_with_sccs : (rv -> VarSet.t Option.t) -> PM.Program.t -> t * scc list Lazy.t
  (** Compute the result variable graph and lazily compute the list of all SCCs
      The first argument computes the variables in the corresponding lsb or None if no such (finite) lsb exists *)

  val rvg_from_transitionset_with_sccs :
    (rv -> VarSet.t Option.t) -> PM.Program.t -> PM.TransitionSet.t -> t * scc list Lazy.t
  (** Similar to [rvg_with_sccs] but only considers transitions from the given [TransitionSet] and its outgoing transitions *)
end
