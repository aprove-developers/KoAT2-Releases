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
  let prf = RankingFunction.find (Program.vars program) (TransitionSet.to_list (TransitionGraph.transitions (Program.graph program))) in
  print_string (RankingFunction.to_string prf)
