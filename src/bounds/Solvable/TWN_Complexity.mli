module Make (Bound : BoundType.Bound) (TL : ProgramTypes.ClassicalTransitionLabelNonFunctionCall) : sig
  module Loop : module type of Loop.Make (Bound) (TL)

  val monotonicity_th_int : int -> int * int -> int * int -> OurInt.t
  (** Computes the monotonicity threshold for k and two tuples (b1,a1) and (b2,a2). *)

  type local_bound = {
    bound : Bound.t;  (** The local bound under the assumption that the loop terminates *)
    terminates : bool Lazy.t;  (** The (expensive) termination proof, only computed when forced *)
  }

  val local_bound :
    ProofOutput.LocalProofOutput.t -> ?twnlog:bool -> ?unsolvable:bool -> Loop.t -> local_bound
  (** Computes the local bound of a twn-loop and defers its termination proof. *)

  val bound_if_terminating : local_bound -> Bound.t
  (** Proves termination and returns the local bound if successful and infinity otherwise. *)
end
