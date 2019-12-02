(* Returns for a list of Bounds b1,...,bn the maximum max(b1,...,bn) *)
val maxBound_of_list : Bound.t list -> Bound.t
(* Returns maximal coefficient for a Multiphase Ranking Function*)
val coefficient : MultiphaseRankingFunction.t -> int