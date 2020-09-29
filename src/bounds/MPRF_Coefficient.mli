open BoundsInst

(** Computes coefficients for Multiphase Ranking Functions necessary for upper time-bounds. *)
(** Handles multiphase ranking functions [cf. Genaim,Amram 2017] and computes coefficients for upper time-bounds. *)

val maxBound_of_list : Bound.t list -> Bound.t
(** Returns for a list of Bounds b1,...,bn the maximum-bound max(b1,...,bn). *)

val coefficient : MultiphaseRankingFunction.t -> int
(** Returns maximal coefficient for a Multiphase Ranking Function [cf. Genaim,Amram 2017]. *)