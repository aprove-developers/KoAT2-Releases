open! OurBase
(** Provides all modules related to result variable graphs. *)

module ModifierComparator : sig
  type 'a comparator_witness
end

module type Adapter2PolyRec = sig
  type t

  val convert : t -> PolyFunctionCall.t
end

module Modifier
    (TL : ProgramTypes.TransitionLabel)
    (_ : Adapter2PolyRec with type t = TL.update_element)
    (T : ProgramTypes.Transition with type transition_label = TL.t) : sig
  type t = T.t GenericModifier_.modifier_t_

  include
    Comparator.S
      with type t := t
       and type comparator_witness = T.comparator_witness ModifierComparator.comparator_witness

  val update : t -> Var.t -> PolyFunctionCall.t
end

(** Module handling result variables. *)
module MakeRV
    (TL : ProgramTypes.TransitionLabel)
    (A : Adapter2PolyRec with type t = TL.update_element)
    (T : ProgramTypes.Transition with type transition_label = TL.t) : sig
  (** Module handling result variables. *)

  type modifier = T.t GenericModifier_.modifier_t_

  include
    ProgramTypes.RV
      with type transition = T.t
       and type modifier := Modifier(TL)(A)(T).t
       and type comparator_witness_modifier = T.comparator_witness ModifierComparator.comparator_witness

  val modifier : t -> modifier
  val to_generic_modifier : modifier -> T.t GenericModifier_.modifier_t_
  val modifier_of_function_call : VarFunctionCall.t -> modifier
  val equal_modifier : modifier -> modifier -> bool
  val update : t -> Var.t -> PolyFunctionCall.t
  val hash : t -> int
  val has_transition : t -> bool
  val is_transition : modifier -> bool
  val function_call : t -> VarFunctionCall.t
  val function_call_ : modifier -> VarFunctionCall.t
end

module IdentityAdapter : sig
  type t = PolyFunctionCall.t

  val convert : t -> PolyFunctionCall.t
end

module RV : module type of MakeRV (TransitionLabel_) (IdentityAdapter) (Transition_)

module Edge : sig
  type t = NORMAL | RETURN

  val default : t
  val compare : t -> t -> int
end

(** Module handling result variable graphs. *)
module MakeRVG (PM : ProgramTypes.ClassicalProgramModules) : sig
  include module type of Graph.Persistent.Digraph.ConcreteBidirectionalLabeled (PM.RV) (Edge)
  (** Module handling result variable graphs, i.e., a digraph where the nodes are result variables. *)

  type rv = PM.RV.t
  type scc = rv list

  val rvs_to_id_string : rv list -> string
  (** Returns a string which is created by calling [to_id_string] on every result variable. *)

  val pre : t -> rv -> rv List.t
  (** Returns the predecessors of a result variable in the result variable graph. *)

  val pre_omega : t -> rv -> rv List.t
  (** Returns the omega-predecessors of a result variable in the result variable graph. *)

  val rvg : (rv -> VarFunctionCallSet.t Option.t) -> PM.Program.t -> t
  (** Compute the result variable graph.
      The first argument computes the variables in the corresponding lsb or None if no such (finite) lsb exists *)

  val rvg_from_transitionset :
    (rv -> VarFunctionCallSet.t Option.t) -> PM.Program.t -> PM.TransitionSet.t -> t
  (** Similar to [rvg] but only considers the transition of the given [TransitionSet] and their outgoing transitions *)

  val rvg_with_sccs : (rv -> VarFunctionCallSet.t Option.t) -> PM.Program.t -> t * scc list Lazy.t
  (** Compute the result variable graph and lazily compute the list of all SCCs
      The first argument computes the variables in the corresponding lsb or None if no such (finite) lsb exists *)

  val rvg_from_transitionset_with_sccs :
    (rv -> VarFunctionCallSet.t Option.t) -> PM.Program.t -> PM.TransitionSet.t -> t * scc list Lazy.t
  (** Similar to [rvg_with_sccs] but only considers transitions from the given [TransitionSet] and its outgoing transitions *)
end
