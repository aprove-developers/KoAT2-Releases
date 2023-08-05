open OurBase

module MakeFromClassical(PM: ProgramTypes.ClassicalProgramModules):
  GraphPrintType.GraphPrint
    with type location = PM.Location.t
     and type transition_label = PM.TransitionLabel.t
     and type transition_comparator_witness = PM.Transition.comparator_witness
     and type program = PM.Program.t

module MakeForRVGFromClassical(PM: ProgramTypes.ClassicalProgramModules): sig
  val print_rvg: label:(RVGTypes.MakeRVG(PM).vertex -> string) -> outdir:Fpath.t -> file:string -> PM.Program.t -> format:string -> unit
end

module ProbabilisticGraphPrint:
  GraphPrintType.GraphPrint
    with type location = ProbabilisticProgramModules.Location.t
     and type transition_label = ProbabilisticProgramModules.TransitionLabel.t
     and type transition_comparator_witness = ProbabilisticProgramModules.Transition.comparator_witness
     and type program = ProbabilisticProgramModules.Program.t

include module type of MakeFromClassical(ProgramModules)
include module type of MakeForRVGFromClassical(ProgramModules)