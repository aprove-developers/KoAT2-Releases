open! OurBase

module MakeForClassicalAnalysis (PM : ProgramTypes.ProgramModules) :
  GraphPrintType.GraphPrint
    with type transition_label = PM.TransitionLabel.t
     and type transition_comparator_witness = PM.Transition.comparator_witness
     and type program = PM.Program.t

module MakeForRVGFromClassical (PM : ProgramTypes.ClassicalProgramModules) : sig
  val print_rvg :
    label:(RVGTypes.MakeRVG(PM).vertex -> string) ->
    outdir:Fpath.t ->
    file:string ->
    PM.Program.t ->
    format:string ->
    unit
end

module MakeForDependencyGraph : sig
  open DependencyGraph

  val print_system : outdir:Fpath.t -> file:string -> DependencyGraph.t -> format:string -> unit
  (** Prints a png file in the given directory with the given filename (the extension .png will be generated) for the transition graph of the program.
          For this operation graphviz need to be installed and the 'dot' command must be accessible in the PATH. *)

  val print_system_pretty : ?file_format:string -> DependencyGraph.t -> string option
  val print_system_pretty_html : DependencyGraph.t -> string
end

module ProbabilisticGraphPrint :
  GraphPrintType.GraphPrint
    with type transition_label = ProbabilisticProgramModules.TransitionLabel.t
     and type transition_comparator_witness = ProbabilisticProgramModules.Transition.comparator_witness
     and type program = ProbabilisticProgramModules.Program.t

include module type of MakeForClassicalAnalysis (ProgramModules)
include module type of MakeForRVGFromClassical (ProgramModules)
