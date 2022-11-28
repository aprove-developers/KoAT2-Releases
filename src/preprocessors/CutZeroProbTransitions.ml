open Batteries
open ProgramTypes

open ProbabilisticProgramModules

(** This preprocessor cuts all transitions with zero probability. *)

let logger = Logging.(get Preprocessor)

let transform_program program =
  let prob_0_trans =
    Program.transitions program
    |> TransitionSet.filter
         ((=) (OurFloat.of_float 0.0) % TransitionLabel.probability % Transition.label)
  in

  TransitionSet.iter
    (fun trans -> Logger.(log logger INFO (fun () -> "cut_zero_prob_transitions",
        ["transition",Transition.to_string trans])))
    prob_0_trans;
  let lift = if TransitionSet.is_empty prob_0_trans then MaybeChanged.same else MaybeChanged.changed in
  lift (Program.remove_transition_set prob_0_trans program)
