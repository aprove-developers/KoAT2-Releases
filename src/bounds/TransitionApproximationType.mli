open Batteries
open BoundsInst
open ProgramModules
(** Abstracts TransitionApproximation so that it can be used to handle normal transitions with integer bounds and general
 * transitions with real bounds*)
module Make(B : BoundType.Bound)
           (PM: ProgramTypes.ProgramModules):
   sig
     type t

     val empty : string -> int -> t

     val get : t -> PM.Transition.t -> B.t

     val get_id : t -> int -> B.t

     (** Returns a timebound of the specified kind for the execution of the whole graph. *)
     val sum : t -> PM.Program.t -> B.t

     val add : ?simplifyfunc:(B.t -> B.t) -> B.t -> PM.Transition.t -> t -> t

     val all_bounded : t -> PM.Transition.t list -> bool

     val to_formatted : ?pretty:bool -> PM.Transition.t list -> t -> FormattedString.t

     val to_string : PM.Transition.t list -> t -> string

     val equivalent : t -> t -> bool
   end
