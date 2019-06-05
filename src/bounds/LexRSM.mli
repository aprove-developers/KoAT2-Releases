open ProgramTypes

val find_whole_prog : Program.t -> string -> unit

type t  
val rank: t -> (Location.t -> Polynomials.RealPolynomial.t)
val decreasing: t -> GeneralTransition.t

(** Returns all non_increasing and decreasing general transitions *)
val non_increasing: t -> GeneralTransitionSet.t

val find : Program.t -> GeneralTransition.t -> t option

val pprf_to_string: t -> string

(* Resets already computed information. Useful for testing purposes *)
val reset: unit -> unit