open Batteries

(** Module type of transitions for which we can create approximations *)
module type ApproximableTransition = sig
  type program
  type t

  val id: t -> int
  val to_id_string: t -> string
  val compare: t -> t -> int
  val all_from_program: program -> t Base.Sequence.t
  val ids_to_string: ?pretty:bool -> t -> string
end

module MakeDefaultApproximableTransition(PM: ProgramTypes.ProgramModules):
  ApproximableTransition with type program = PM.Program.t
                          and type t = PM.Transition.t

(** Abstracts TransitionApproximation so that it can be used to handle normal transitions with integer bounds and general
 * transitions with real bounds*)
module Make(B : BoundType.Bound)
           (T: ApproximableTransition):
   sig
     type t

     val empty : string -> int -> t

     val get : t -> T.t -> B.t

     (** Returns a timebound for the execution of all given transitions *)
     val sum : t -> T.program -> B.t

     val add : ?simplifyfunc:(B.t -> B.t) -> B.t -> T.t -> t -> t

     val all_bounded : t -> T.t Enum.t -> bool

     val to_formatted : ?pretty:bool -> ?termination_only:bool -> T.t list -> t -> FormattedString.t

     val to_string : ?termination_only:bool -> T.t list -> t -> string

     val equivalent : t -> t -> bool
   end

module EqMake(B: BoundType.Bound)
             (T: ApproximableTransition)(T': ApproximableTransition): sig
  val proof: (Make(B)(T).t, Make(B)(T').t) Util.TypeEq.t
end
