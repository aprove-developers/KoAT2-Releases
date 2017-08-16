open Batteries

(** Provides all necessary types for constraints with basic polynomials *)

module StdConstraint = Constraints.Make(StdPoly.Polynomial)
