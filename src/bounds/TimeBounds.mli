open Batteries

(** Provides default implementations of TimeBounds *)

module Make(A : BoundTypes.Approximation) : BoundTypes.TimeBounds with   
         module Approximation_ = A
