open ProgramTypes

val find_whole_prog : Program.t -> string -> unit

type t  
val rank: t -> (Location.t -> Polynomials.RealPolynomial.t)
val decreasing: t -> GeneralTransition.t
val non_increasing: t -> GeneralTransitionSet.t

val find : Program.t -> GeneralTransition.t -> t option

val pprf_to_string: t -> string
