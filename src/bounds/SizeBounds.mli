open Batteries

(** Provides default implementations of SizeBounds *)

module Make(A : BoundTypes.Approximation) : BoundTypes.SizeBounds with   
         module Approximation_ = A
