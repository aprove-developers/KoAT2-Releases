module Make (Bound : BoundType.Bound) (PM : ProgramTypes.ClassicalProgramModules) : sig
  module Loop : module type of Loop.Make (Bound) (PM)

  val monotonicity_th_int : int -> int * int -> int * int -> OurInt.t
  (** Computes the monotonicity threshold for k and two tuples (b1,a1) and (b2,a2). *)

  val complexity :
    ProofOutput.LocalProofOutput.t ->
    ?entry:PM.Transition.t option ->
    ?termination:bool ->
    ?unsolvable:bool ->
    Loop.t ->
    Bound.t
  (** This method computes a complexity bound for twn-loops.
    Gets a {[Loop]}, an entry {[Transition]} for invariants derivation in the termination proof, and a closed form of every variable. *)
end
