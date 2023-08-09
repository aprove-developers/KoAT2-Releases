open ProgramModules

val improve : Program.t -> ?scc:TransitionSet.t option -> Approximation.t -> Approximation.t
val reset_cfr : unit -> unit
