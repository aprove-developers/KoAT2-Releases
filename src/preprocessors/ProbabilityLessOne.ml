open Batteries
open ProgramTypes
open Polynomials

(** This preprocessor creates additional transitions leading to a sink in order to ensure that the total probability of a general tranistion is 1*)

let logger = Logging.(get Preprocessor)

let check_program trans_id_counter program =
  let trans_prob_less_1 =
    program
    |> Program.generalized_transitions
    |> GeneralTransitionSet.filter (fun gen_trans -> GeneralTransition.total_probability gen_trans < (1. |> OurFloat.of_float))
  in
  if GeneralTransitionSet.is_empty trans_prob_less_1 then
    MaybeChanged.same program
  else
    let loc_name_list = program |> Program.graph |> TransitionGraph.locations |> LocationSet.to_list |> List.map (Location.to_string) in
    let i = ref 0 in
    while List.mem ("s_" ^ (Int.to_string !i)) loc_name_list do i := (!i+1) done;
    let new_sink = "s_" ^ (Int.to_string !i) |> Location.of_string in
    let identity_update =
    (*get the update for all new transitions*)
      program
      |> Program.vars
      |> VarSet.to_list
      |> List.fold_left (fun varmap var -> TransitionLabel.VarMap.add var
                          (TransitionLabel.UpdateElement.Poly (Polynomial.of_var var)) varmap)
         TransitionLabel.VarMap.empty
    in

    let new_transitions =
      trans_prob_less_1
      |> GeneralTransitionSet.to_list
      |> List.map (fun trans : Transition.t ->   (GeneralTransition.start trans,
                                  TransitionLabel.make_prob trans_id_counter
                                      "Com_1"
                                      ~guard:(Constraints.Constraint.mk_true)
                                      ~gt_id:(GeneralTransition.id trans)
                                      ~update:(identity_update)
                                      ~probability:(OurFloat.(-) (OurFloat.of_float 1.) (GeneralTransition.total_probability trans)),
                                  new_sink))
    in

    program
    |> Program.transitions
    |> TransitionSet.to_list
    |> fun list -> [list; new_transitions]
    |> List.concat
    |> fun transitions -> Program.from transitions (Program.start program) (Program.program_vars_ordered program)
    |> MaybeChanged.changed

