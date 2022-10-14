open Batteries
open ProgramModules
open ApproximationModules

(** Generates the approximation for the new program_cfr from the one of the original program. *)
let merge_appr (program: Program.t) (program_cfr: Program.t) appr =
  let unchangend_trans = TransitionSet.inter (Program.transitions program) (Program.transitions program_cfr) in
  program_cfr
  |> Approximation.create
  |> TrivialTimeBounds.compute program_cfr
  |> TransitionSet.fold (fun trans appr_cfr ->
                                      let timebound = appr
                                      |> Approximation.time
                                      |> flip TransitionApproximation.get trans and
                                        costbound = appr
                                      |> Approximation.cost
                                      |> flip TransitionApproximation.get trans in
                                      VarSet.fold (fun x appr_cfr ->
                                              let sizebound_x = SizeApproximation.get (Approximation.size appr) trans x in
                                                appr_cfr
                                                |> Approximation.add_sizebound sizebound_x trans x) (Program.vars program) appr_cfr

                                      |> Approximation.add_timebound timebound trans
                                      |> Approximation.add_costbound costbound trans) unchangend_trans

let lift_to_program transform program =
  MaybeChanged.(transform (Program.graph program) >>= (fun graph -> same (Program.map_graph (fun _ -> graph) program)))

