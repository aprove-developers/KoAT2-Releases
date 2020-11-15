open ProgramTypes

type lrsm_cache
val new_cache: unit -> lrsm_cache

type t
val rank: t -> (Location.t -> Polynomials.RealPolynomial.t)
val decreasing: t -> GeneralTransition.t

(** Returns all non_increasing and decreasing general transitions *)
val non_increasing: t -> GeneralTransitionSet.t

val find : ?refined:bool -> ?timeout:float option -> lrsm_cache -> Program.t -> GeneralTransition.t -> t option

val pprf_to_string: t -> string

