open Batteries
open Program.Types
   
let description = "Search for a linear ranking function"

let command = "prf"

type params = {
  input : string; [@pos 0] [@docv "INPUT"]
  (** An absolute or relative path to the koat input file which defines the integer transition system *)
  } [@@deriving cmdliner, show]
  
let run (params: params) =
  let program = Readers.read_file params.input in
  let empty_approx = Approximation.empty (program |> Program.graph |> TransitionGraph.transitions |> TransitionSet.cardinal) (program |> Program.vars |> VarSet.cardinal) in
  let (_,(program_procs,approx_trivial_tb)) = TrivialTimeBounds.transform (program, empty_approx) in
  let transitions = List.filter (fun trans -> Bound.is_infinity (Approximation.timebound approx_trivial_tb trans)) (TransitionSet.to_list (TransitionGraph.transitions (Program.graph program_procs))) in
  let prf = RankingFunction.find (Program.vars program_procs) transitions in
  print_string (RankingFunction.to_string prf ^ "\n")
