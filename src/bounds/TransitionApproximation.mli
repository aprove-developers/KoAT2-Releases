open Batteries
open ProgramTypes
   
type t

val empty : string -> int -> t

val get : t -> Transition.t -> Bound.t

(** Returns a timebound of the specified kind for the execution of the whole graph. *)
val sum : t -> Program.t -> Bound.t

val add : Bound.t -> Transition.t -> t -> t

val all_bounded : t -> Transition.t list -> bool

val get_between : t -> Location.t -> Location.t -> Bound.t Option.t

val to_string : TransitionSet.t -> t -> string

val equivalent : t -> t -> bool
