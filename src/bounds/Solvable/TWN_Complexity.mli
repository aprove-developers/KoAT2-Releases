open Atoms
open BoundsInst

module Make(PM: ProgramTypes.ClassicalProgramModules): sig
  module Loop: module type of Loop.Make(PM)

  (** Computes the monotonicity threshold for k and two tuples (b1,a1) and (b2,a2). *)
  val monotonicity_th_int : int -> (int * int) -> (int * int) -> OurInt.t

  (** This method computes a complexity bound for twn-loops.
    Gets a {[Loop]}, an entry {[Transition]} for invariants derivation in the termination proof, and a closed form of every variable. *)
  val complexity : ?entry:(PM.Transition.t) option -> Atom.t list -> Loop.t -> Bound.t

  (** Gets a {[TransitionLabel]} in twn-form and returns a complexity bound for the respective twn-loop. *)
  val complexity_ : PM.Transition.t -> Bound.t
end
