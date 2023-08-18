open OurBase

(** Module type of transitions for which we can create approximations *)
module type ApproximableTransition = sig
  type program
  type t

  val id : t -> int
  val to_id_string : t -> string
  val compare : t -> t -> int
  val all_from_program : program -> t Sequence.t
  val ids_to_string : ?pretty:bool -> t -> string

  include Comparator.S with type t := t
end

module MakeDefaultApproximableTransition (PM : ProgramTypes.ProgramModules) :
  ApproximableTransition
    with type program = PM.Program.t
     and type t = PM.Transition.t
     and type comparator_witness = PM.Transition.comparator_witness

type ('trans, 'bound, 'trans_cmp_wit) transition_approximation_t
(** The type of transition approximations *)

(** Abstracts TransitionApproximation so that it can be used to handle normal transitions with integer bounds and general
 * transitions with real bounds*)
module Make (B : BoundType.Bound) (T : ApproximableTransition) : sig
  type t = (T.t, B.t, T.comparator_witness) transition_approximation_t

  val empty : string -> t
  val get : t -> T.t -> B.t

  val sum : t -> T.program -> B.t
  (** Returns a timebound for the execution of all given transitions *)

  val add : ?simplifyfunc:(B.t -> B.t) -> B.t -> T.t -> t -> t
  val all_bounded : t -> T.t Sequence.t -> bool
  val to_formatted : ?pretty:bool -> ?termination_only:bool -> T.t list -> t -> FormattedString.t
  val to_string : ?termination_only:bool -> T.t list -> t -> string
end
