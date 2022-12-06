(** Provides all modules related to result variable graphs. *)
open Batteries

module type RVType = sig
  (** The transition part of the RVG, i.e., in case of classical programs transitions and in case of probabilistic programs tuples of transitions and locations *)
  type transition

  (** Type of a result variable is a transiton and a variable. *)
  type t = transition * Var.t

  val to_id_string: t -> string
  val same: t -> t -> bool
  val hash: t -> int
  val compare_same: t -> t -> int
  val ids_to_string: ?pretty:bool -> t -> string
end

(** Module handling result variables. *)
module MakeRV(TL: ProgramTypes.TransitionLabel)
             (T: ProgramTypes.Transition with type transition_label = TL.t): sig
  (** Module handling result variables. *)

  include RVType with type transition = T.t

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

module RV: module type of MakeRV(ProgramModules.TransitionLabel)(ProgramModules.Transition)

(** Module handling result variable graphs. *)
module MakeRVG(PM: ProgramTypes.ClassicalProgramModules):
sig
  include module type of Graph.Persistent.Digraph.ConcreteBidirectional(struct
                             include MakeRV(PM.TransitionLabel)(PM.Transition)
                             let equal = same
                             let compare = compare_same
                           end)
  (** Module handling result variable graphs, i.e., a digraph where the nodes are result variables. *)

  (** Returns a string which is created by calling [to_id_string] on every result variable. *)
  val rvs_to_id_string : MakeRV(PM.TransitionLabel)(PM.Transition).t list -> string

  (** Returns the predecessors of a result variable in the result variable graph. *)
  val pre : t -> MakeRV(PM.TransitionLabel)(PM.Transition).t -> MakeRV(PM.TransitionLabel)(PM.Transition).t Enum.t

  type scc = MakeRV(PM.TransitionLabel)(PM.Transition).t list

  (** Compute the result variable graph *)
  val rvg : PM.Program.t -> t

  (** Compute the result variable graph and lazily compute the list of all SCCs *)
  val rvg_with_sccs : PM.Program.t -> t * scc list Lazy.t
end

module RVG: module type of MakeRVG(ProgramModules)
