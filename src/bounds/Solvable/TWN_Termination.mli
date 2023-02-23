open Atoms
open PolyExponential

module Make(PM: ProgramTypes.ClassicalProgramModules): sig
  module Loop: module type of Loop.Make(PM)

  (** This method proves termination of twn-loops.
    Gets a {[Loop]}, an entry {[Transition]} for invariants derivation, and a closed form of every variable. *)
  val termination_ : ?entry:(PM.Transition.t) option -> Loop.t -> Atom.t list -> (Var.t, PE.t) Hashtbl.t -> bool

  (** Gets a {[TransitionLabel]} in twn-form and returns true iff a twn-termination proof was successful. *)
  val termination : PM.Transition.t -> bool
end
