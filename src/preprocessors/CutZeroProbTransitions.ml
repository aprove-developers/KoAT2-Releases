open OurBase
open ProgramTypes
open ProbabilisticProgramModules

(** This preprocessor cuts all transitions with zero probability. *)

let logger = Logging.(get Preprocessor)

let transform_program program =
  let prob_0_trans =
    Program.transitions program
    |> Set.filter ~f:OurFloat.(equal zero % TransitionLabel.probability % Transition.label)
  in

  Set.iter
    ~f:(fun trans ->
      Logger.(
        log logger INFO (fun () ->
            ("cut_zero_prob_transitions", [ ("transition", Transition.to_string trans) ]))))
    prob_0_trans;
  let lift =
    if Set.is_empty prob_0_trans then
      MaybeChanged.same
    else
      MaybeChanged.changed
  in
  lift (Program.remove_zero_prob_transitions prob_0_trans program)
