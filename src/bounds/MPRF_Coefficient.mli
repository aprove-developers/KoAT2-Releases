open BoundsInst

(** Computes coefficients for Multiphase Ranking Functions necessary for upper time-bounds. *)
(** Handles multiphase ranking functions (see [Ben-Amram and Genaim 2017]) and computes coefficients for upper time-bounds. *)

val sumBound_of_list : Bound.t list -> Bound.t
(** Returns for a list of Bounds b1,...,bn the sum of bounds b1 + ... + bn. *)

val coefficient : MultiphaseRankingFunction.t -> int
(** Returns maximal coefficient for a Multiphase Ranking Function as in [Ben-Amram and Genaim 2017]. *)