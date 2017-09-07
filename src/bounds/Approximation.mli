open Batteries

(** Provides default implementations of an approximation *)

module Make(G : TransitionGraphTypes.TransitionGraph) : BoundTypes.Approximation with   
         module TransitionGraph_ = G
