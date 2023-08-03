open OurBase

(** Module type of transitions for which we can create approximations *)
module type ApproximableTransition = sig
  type program
  type t

  val id: t -> int
  val to_id_string: t -> string
  val compare: t -> t -> int
  val all_from_program: program -> t Base.Sequence.t
  val ids_to_string: ?pretty:bool -> t -> string

  val sexp_of_t: t -> Sexp.t
  val hash: t -> int
end

(** The type of transition approximations *)
type ('trans,'bound) transition_approximation_t

module MakeDefaultApproximableTransition(PM: ProgramTypes.ProgramModules):
  ApproximableTransition with type program = PM.Program.t
                          and type t = PM.Transition.t

(** Abstracts TransitionApproximation so that it can be used to handle normal transitions with integer bounds and general
 * transitions with real bounds*)
module Make(B : BoundType.Bound)
           (T: ApproximableTransition):
   sig
     type t = (T.t,B.t) transition_approximation_t

     val empty : string -> int -> t

     val get : t -> T.t -> B.t

     (** Returns a timebound for the execution of all given transitions *)
     val sum : t -> T.program -> B.t

     val add : ?simplifyfunc:(B.t -> B.t) -> B.t -> T.t -> t -> t

     val all_bounded : t -> T.t Sequence.t -> bool

     val to_formatted : ?pretty:bool -> ?termination_only:bool -> T.t list -> t -> FormattedString.t

     val to_string : ?termination_only:bool -> T.t list -> t -> string
   end
