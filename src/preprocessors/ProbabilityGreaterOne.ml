open Batteries
open ProgramTypes

(** This preprocessor throws an error if the total Probability of a Generalized Transition is greater than one. *)

let logger = Logging.(get Preprocessor)

let check_program program =
  let trans_prob_greater_1 =
    program
    |> Program.generalized_transitions
    |> GeneralTransitionSet.filter (fun gen_trans -> 
      (TransitionSet.fold (fun trans rest -> (trans |> Transition.label |> TransitionLabel.probability) +. rest) (GeneralTransition.transitions gen_trans) 0.)> 1.)
  in 
  if GeneralTransitionSet.is_empty trans_prob_greater_1 then
    MaybeChanged.same program
  else
    raise (Failure "Generalized Transition with probability greater than 1");

