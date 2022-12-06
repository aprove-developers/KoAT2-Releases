open Batteries
open ProgramModules

(** Generates the approximation for the new program_cfr from the one of the original program. *)
let merge_appr (program: Program.t) (program_cfr: Program.t) appr =
  let unchangend_trans = TransitionSet.inter (Program.transitions program) (Program.transitions program_cfr) in
  program_cfr
  |> Approximation.create
  |> TrivialTimeBounds.compute program_cfr
  |> TransitionSet.fold (fun trans appr_cfr ->
                                      let timebound = appr
                                      |> flip Approximation.timebound trans and
                                        costbound = appr
                                      |> flip Approximation.costbound trans in
                                      VarSet.fold (fun x appr_cfr ->
                                              let sizebound_x = Approximation.sizebound appr  trans x in
                                                appr_cfr
                                                |> Approximation.add_sizebound sizebound_x trans x) (Program.vars program) appr_cfr

                                      |> Approximation.add_timebound timebound trans
                                      |> Approximation.add_costbound costbound trans) unchangend_trans

let lift_to_program transform program =
  MaybeChanged.(transform (Program.graph program) >>= (fun graph -> same (Program.map_graph (fun _ -> graph) program)))

