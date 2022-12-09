open ProbabilisticProgramModules

type lrsm_cache
val new_cache: unit -> lrsm_cache

type t
val rank: t -> (Location.t -> Polynomials.RealPolynomial.t)
val decreasing: t -> GeneralTransition.t

(** Returns all non_increasing and decreasing general transitions *)
val non_increasing: t -> GeneralTransitionSet.t

val find_scc : ?refined:bool -> ?timeout:float option -> Program.t
            -> (GeneralTransition.t -> bool) (* Is the general transition already time bounded? *)
            -> (GeneralTransition.t * Location.t -> VarSet.t) (* Variables with unbounded expected sizes *)
            -> GeneralTransitionSet.t -> GeneralTransition.t -> t option

val find : ?refined:bool -> ?timeout:float option -> Program.t -> GeneralTransition.t -> t option

val to_string: t -> string

