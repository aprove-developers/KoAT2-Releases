open Batteries
open ProgramModules

(** Generates the approximation for the new program_cfr from the one of the original program. *)
let merge_appr (program: Program.t) (program_cfr: Program.t) appr =
  let unchanged_trans = Base.Set.inter (Program.transitions program) (Program.transitions program_cfr) in
  let appr =
    Approximation.empty
    |> TrivialTimeBounds.compute program_cfr
  in
  unchanged_trans
  |> Base.Set.fold ~f:(fun appr_cfr trans ->
                                      let timebound = appr
                                      |> flip Approximation.timebound trans and
                                        costbound = appr
                                      |> flip Approximation.costbound trans in
                                      Base.Set.fold ~f:(fun appr_cfr x ->
                                              let sizebound_x = Approximation.sizebound appr  trans x in
                                                appr_cfr
                                                |> Approximation.add_sizebound sizebound_x trans x) (Program.vars program) ~init:appr_cfr

                                      |> Approximation.add_timebound timebound trans
                                      |> Approximation.add_costbound costbound trans) ~init:appr

let lift_to_program transform program =
  MaybeChanged.(transform (Program.graph program) >>= (fun graph -> same (Program.map_graph (fun _ -> graph) program)))

