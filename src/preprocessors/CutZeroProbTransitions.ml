open Batteries
open ProgramTypes

(** This preprocessor cuts all transitions with zero probability. *)

let logger = Logging.(get Preprocessor)

let transform_program program =
  let trans_prob_0 =
    TransitionGraph.transitions (Program.graph program)
    |> TransitionSet.filter
         ((=) (OurFloat.of_float 0.0) % TransitionLabel.probability % Transition.label) in
  if TransitionSet.is_empty trans_prob_0 then
    MaybeChanged.same program
  else
    let remove transition program =
      Logger.(log logger INFO (fun () -> "cut_zero_prob_transitions",
        ["transition",Transition.to_string ~show_gtcost:false transition]));
      Program.remove_transition program transition
    in
    MaybeChanged.changed (TransitionSet.fold remove trans_prob_0 program)
