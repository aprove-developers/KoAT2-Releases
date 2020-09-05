open ProgramTypes
open Batteries
open Logger

(** All entry transitions of the given transitions.
    These are such transitions, that can occur immediately before one of the transitions, but are not themselves part of the given transitions. *)
let entry_transitions pre_cache (logger: Logger.log) (program: Program.t) (rank_transitions: GeneralTransition.t list): ((GeneralTransition.t * Location.t) Enum.t) =
  let gts =
    Program.generalized_transitions program |> GeneralTransitionSet.to_list
  in
  let single_entry_transitions =
    rank_transitions
    |> List.enum
    |> Enum.map (Program.pre pre_cache program % TransitionSet.any % GeneralTransition.transitions)
    |> Enum.flatten
    |> Enum.filter (fun r ->
           rank_transitions
           |> List.enum
           |> Enum.map (TransitionSet.enum % GeneralTransition.transitions)
           |> Enum.flatten
           |> Enum.for_all (not % Transition.same r)
         )
    |> Enum.uniq_by Transition.same
  in
  single_entry_transitions
  |> Enum.map (fun transition ->
                 (List.find (TransitionSet.mem transition % GeneralTransition.transitions) gts, Transition.target transition) )
  |> Enum.uniq_by (fun (gt1,l1) (gt2,l2) -> GeneralTransition.same gt1 gt2 && Location.equal l1 l2)
  |> tap (fun transitions -> Logger.log logger Logger.DEBUG
                               (fun () -> "entry_transitions",
                               [ "result", transitions
                                           |> Enum.clone |> Util.enum_to_string
                                           (fun (gt,l) -> "(" ^ GeneralTransition.to_id_string gt ^ ", " ^ Location.to_string l ^ ")")
                               ; "transitions", rank_transitions |> List.enum |> Util.enum_to_string GeneralTransition.to_id_string]))
