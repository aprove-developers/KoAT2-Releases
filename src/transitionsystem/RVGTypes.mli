(** Provides all modules related to result variable graphs. *)
open OurBase

(** Module handling result variables. *)
module MakeRV(TL: ProgramTypes.TransitionLabel)
             (T: ProgramTypes.Transition with type transition_label = TL.t): sig
  (** Module handling result variables. *)

  include ProgramTypes.RV with type RVTuple_.transition = T.t

  (** TODO doc *)
  val equivalent : t -> t -> bool

  (** TODO doc *)
  val compare_equivalent : t -> t -> int

  (** Returns a string representing a result variable consisting of the transition-id string and the variable string. *)
  val to_id_string : t -> string

  (** Returns the transition of the result variable. *)
  val transition : t -> T.t

  (** Returns the variable of the result variable. *)
  val variable : t -> Var.t
end

module RV: module type of MakeRV(TransitionLabel_)(Transition_)

(** Module handling result variable graphs. *)
module MakeRVG(PM: ProgramTypes.ClassicalProgramModules):
sig
  include module type of Graph.Persistent.Digraph.ConcreteBidirectional(MakeRV(PM.TransitionLabel)(PM.Transition))
  (** Module handling result variable graphs, i.e., a digraph where the nodes are result variables. *)

  (** Returns a string which is created by calling [to_id_string] on every result variable. *)
  val rvs_to_id_string : MakeRV(PM.TransitionLabel)(PM.Transition).t list -> string

  (** Returns the predecessors of a result variable in the result variable graph. *)
  val pre : t -> MakeRV(PM.TransitionLabel)(PM.Transition).t -> MakeRV(PM.TransitionLabel)(PM.Transition).t List.t

  type scc = MakeRV(PM.TransitionLabel)(PM.Transition).t list

  (** Compute the result variable graph. *)
  (** The first argument computes the variables in the corresponding lsb or None if no such (finite) lsb exists *)
  val rvg : (MakeRV(PM.TransitionLabel)(PM.Transition).t -> VarSet.t Option.t) -> PM.Program.t -> t

  (** Compute the result variable graph and lazily compute the list of all SCCs *)
  (** The first argument computes the variables in the corresponding lsb or None if no such (finite) lsb exists *)
  val rvg_with_sccs : (MakeRV(PM.TransitionLabel)(PM.Transition).t -> VarSet.t Option.t) -> PM.Program.t -> t * scc list Lazy.t
end
