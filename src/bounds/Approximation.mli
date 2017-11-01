open Batteries

(** Provides default implementations of an approximation *)

type t 
   
(** Distinguish between lower and upper bounds *)
type kind = [ `Lower | `Upper ]


module Time :
sig
  type t
  val empty : int -> t
  val get : t -> Program.Transition.t -> Bound.t
  (** Returns a timebound of the specified kind for the execution of the whole graph. *)
  val sum : t -> Program.t -> Bound.t
  val add : Bound.t -> Program.Transition.t -> t -> t
  val all_bounded : t -> Program.Transition.t list -> bool
  val to_string : t -> string
  val equal : t -> t -> bool
end

module Size :
sig
  type t
  val empty : int -> t
  val get : kind -> t -> Program.Transition.t -> Var.t -> Bound.t
  val add : kind -> Bound.t -> Program.Transition.t -> Var.t -> t -> t
  val add_all : kind -> Bound.t -> Program.RVG.scc -> t -> t
  val to_string : t -> string
  val equal : t -> t -> bool
end

(** Returns an empty approximation that does not contain any non-trivial information.
        That means, that every upper bound is infinite and every lower bound is minus infinite.
        The first parameter should be the count of transitions in the program.
        The second parameter should be the count of program variables. *)
val empty : int -> int -> t

val time : t -> Time.t

val size : t -> Size.t

(** Timebound related methods *)
  
(** Returns a timebound for the transition. *)
val timebound : t -> Program.Transition.t -> Bound.t

(** Returns a timebound for the unique transition from the source to the target or None if there isn't such a single transition. *)
val timebound_between : t -> Program.Location.t -> Program.Location.t -> Bound.t Option.t

(** Adds the information that the specified bound is a valid timebound for the given transition. 
        The resulting approximation is guaranteed to be at least as good as the old approximation. *)
val add_timebound : Bound.t -> Program.Transition.t -> t -> t

val all_times_bounded : t -> Program.Transition.t list -> bool
  

(** Sizebound related methods *)

(** Returns a sizebound of the specified kind for the var of the transition. 
        A sizebound is expressed in relation to the input variable values of the program. *)
val sizebound : kind -> t -> Program.Transition.t -> Var.t -> Bound.t

(** Adds the information that the specified bound is a valid sizebound for the given variable of the transition. 
        The resulting approximation is guaranteed to be at least as good as the old approximation. *)
val add_sizebound : kind -> Bound.t -> Program.Transition.t -> Var.t -> t -> t
  
val add_sizebounds : kind -> Bound.t -> Program.RVG.scc -> t -> t

val to_string : Program.t -> t -> string

val equal : t -> t -> bool
