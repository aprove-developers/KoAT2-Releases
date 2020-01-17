open Batteries
open BoundsInst
open ProgramTypes
open ApproximationModules


(** Provides default implementations of an approximation *)

type t

(** Distinguish between lower and upper bounds *)
type kind = [ `Lower | `Upper ]

(** Returns an empty approximation that does not contain any non-trivial information.
    That means, that every upper bound is infinite and every lower bound is minus infinite.
    The first parameter should be the count of transitions in the program.
    The second parameter should be the count of program variables. *)
val empty : int -> int -> int -> t

val create : Program.t -> t

val time : t -> TransitionApproximation.t

val exptime : t -> GeneralTransitionApproximation.t

val size : t -> SizeApproximation.t

val cost : t -> TransitionApproximation.t

val expcost : t -> GeneralTransitionApproximation.t

val to_string : Program.t -> bool -> t -> string

val equivalent : t -> t -> bool


(** Timebound related methods *)

(** Returns a timebound for the transition. *)
val timebound : t -> Transition.t -> Bound.t


(** Returns a (Nonprobabilistic) timebound for the transition. *)
val timebound_gt : t -> GeneralTransition.t -> Bound.t

(** Returns an expected timebound for the transition. *)
val exptimebound : t -> GeneralTransition.t -> RealBound.t

(** Returns a timebound for the transition id. *)
val timebound_id : t -> int -> Bound.t

(** Returns a timebound for the program. *)
val program_timebound : t -> Program.t -> Bound.t

(** Returns the expected timebound for the whole program. *)
val program_exptimebound : t -> Program.t -> RealBound.t

(** Adds the information that the specified bound is a valid timebound for the given transition.
    The resulting approximation is guaranteed to be at least as good as the old approximation. *)
val add_timebound : Bound.t -> Transition.t -> t -> t

(** Adds the information that the specified bound is a valid (Nonprobabilistic!) timebound for the given general transition.
    This is useful since sometimes we require that a general transition is executed at most once. Hence
    this bound is more accurate then summing up the 'normal' timebounds.
    The resulting approximation is guaranteed to be at least as good as the old approximation. *)
val add_timebound_gt : Bound.t -> GeneralTransition.t -> t -> t

(** Adds the information that the specified bound is a valid expected timebound for the given general transition.
    The resulting approximation is guaranteed to be at least as good as the old approximation.
    The first parameter describes if the bound should be simplified using the SMT Solver*)
val add_exptimebound : bool -> RealBound.t -> GeneralTransition.t -> t -> t

val all_times_bounded : t -> Transition.t list -> bool

val is_time_bounded : t -> Transition.t -> bool

val is_exptime_bounded : t -> GeneralTransition.t -> bool

val is_expcost_bounded : t -> GeneralTransition.t -> bool

(** Costbound related methods *)

(** Returns a costbound for the transition. *)
val costbound : t -> Transition.t -> Bound.t

(** Returns an expected costbound for the given general transition. *)
val expcostbound : t -> GeneralTransition.t -> RealBound.t

(** Returns a costbound for the program. *)
val program_costbound : t -> Program.t -> Bound.t

(** Returns an expected costbound for the program. *)
val program_expcostbound : t -> Program.t -> RealBound.t

val add_costbound : Bound.t -> Transition.t -> t -> t

(** The first parameter describes if the bound should be simplified using the SMT Solver *)
val add_expcostbound : bool -> RealBound.t -> GeneralTransition.t -> t -> t


(** Sizebound related methods *)

(** Returns a sizebound of the specified kind for the var of the transition.
        A sizebound is expressed in relation to the input variable values of the program. *)
val sizebound : kind -> t -> Transition.t -> Var.t -> Bound.t

(** Returns an expected sizebound of the specified kind for the var of the transition.
        A sizebound is expressed in relation to the input variable values of the program. *)
val expsizebound : kind -> t -> (GeneralTransition.t * Location.t)-> Var.t -> RealBound.t

(** Returns an expected sizebound that bounds the absolute value of the given variable.
        A sizebound is expressed in relation to the input variable values of the program. *)
val expsizebound_abs : t -> (GeneralTransition.t * Location.t)-> Var.t -> RealBound.t

(** Adds the information that the specified bound is a valid sizebound for the given variable of the transition.
        The resulting approximation is guaranteed to be at least as good as the old approximation. *)
val add_sizebound : kind -> Bound.t -> Transition.t -> Var.t -> t -> t

(** The first parameter describes if the bound should be simplified using the SMT Solver *)
val add_expsizebound : bool -> RealBound.t -> GeneralTransition.t * Location.t -> Var.t -> t -> t

val add_sizebounds : kind -> Bound.t -> RV.t list -> t -> t

(** The first parameter describes if the bound should be simplified using the SMT Solver *)
val add_expsizebounds : bool -> RealBound.t -> ERV.t list -> t -> t
