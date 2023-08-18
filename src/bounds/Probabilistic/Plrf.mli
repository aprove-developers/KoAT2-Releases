open ProbabilisticProgramModules

type lrsm_cache

val new_cache : unit -> lrsm_cache

type t

val rank : t -> Location.t -> Polynomials.RationalPolynomial.t
val decreasing : t -> GeneralTransition.t

val non_increasing : t -> GeneralTransitionSet.t
(** Returns all non_increasing and decreasing general transitions *)

val find_scc :
  ?refined:bool ->
  ?timeout:float option ->
  Program.t ->
  (GeneralTransition.t * Location.t -> bool) ->
  (* Is the general transition already time bounded? The target location of the general transition is provided to utilise better utilise classic time bounds *)
  (GeneralTransition.t * Location.t -> VarSet.t) ->
  (* Variables with unbounded expected sizes *)
  GeneralTransitionSet.t ->
  GeneralTransition.t ->
  t option

val find : ?refined:bool -> ?timeout:float option -> Program.t -> GeneralTransition.t -> t option
val to_string : t -> string

val compute_proof : t -> Bounds.RealBound.t -> Program.t -> Formatter.format -> FormattedString.t
(** This function computes an explanation for the given PRF and the given resulting bound *)
