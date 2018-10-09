open ProgramTypes

val find_whole_prog : Program.t -> string -> unit

type t = {
    rank : Location.t -> Polynomials.Polynomial.t;
    decreasing : GeneralTransition.t;
    non_increasing : GeneralTransitionSet.t;
  }

val find : Program.t -> GeneralTransition.t -> t option
