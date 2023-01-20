open ProgramModules

val improve : Program.t -> ?scc:TransitionSet.t option -> Approximation.t -> Approximation.t
