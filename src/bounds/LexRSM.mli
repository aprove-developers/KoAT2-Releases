open ProgramTypes

val find_whole_prog : Program.t -> string -> unit

type t = {
    rank : Location.t -> Polynomials.Polynomial.t;
    decreasing : GeneralTransition.t;
    non_increasing : GeneralTransitionSet.t;
  }

val rank: t -> (Location.t -> Polynomials.Polynomial.t)
val decreasing: t -> GeneralTransition.t
val non_increasing: t -> GeneralTransitionSet.t

val find : Program.t -> GeneralTransition.t -> t option

val pprf_to_string: t -> string
