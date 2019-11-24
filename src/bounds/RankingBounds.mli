open Batteries

(** Performs a single improvement step to find better timebounds for the approximation and updates the approximation. *)
val improve : RankingFunction.ranking_cache -> [ `Cost | `Time ] -> Program.t -> Approximation.t -> Approximation.t MaybeChanged.t
