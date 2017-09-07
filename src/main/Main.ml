open Batteries

module Var_ = ID.StringID
module Value_ = Number.MakeNumeric(Big_int)
module Polynomial_ = Polynomials.Make(Var_)(Value_)
module Constraint_ = Constraints.Make(Polynomial_)
module Transition_ = TransitionGraph.MakeTransition(Constraint_)
module TransitionGraph_ = TransitionGraph.MakeTransitionGraph(Transition_)
module Approximation_ = Approximation.Make(TransitionGraph_)

module SMT_ = SMT.MakeZ3Solver(Constraint_)                      

module Reader_ = Readers.Make(TransitionGraph_)
            

let preprocessors: (TransitionGraph_.t -> TransitionGraph_.t) list = []

(* We apply each preprocessor exactly one time *)
let preprocess (graph: TransitionGraph_.t): TransitionGraph_.t =
  List.fold_left (fun graph preprocessor -> preprocessor graph) graph preprocessors

let find_bounds (graph: TransitionGraph_.t): Approximation_.t =
  raise (Failure "Not yet implemented")

let print_results (appr: Approximation_.t): unit =
  raise (Failure "Not yet implemented")  

let () =
  let file = "file" in
     Reader_.read_file file
  |> preprocess
  |> find_bounds
  |> print_results

