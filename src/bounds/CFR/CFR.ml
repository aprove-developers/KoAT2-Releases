open! OurBase

(* Timeouts *)
(* Measures the time spend on CFR. *)
let time_cfr = ref 180.

module CFR (Bound : BoundType.Bound) = struct
  open ProgramModules
  module Approximation = Approximation.MakeForClassicalAnalysis (Bound) (ProgramModules)
  module TrivialTimeBounds = TrivialTimeBounds.Make (Bound) (ProgramModules)

  type approximation = Approximation.t

  let time_cfr = time_cfr

  (* timeout time_left_cfr * |scc| / |trans_left and scc| or inf if ex. unbound transition in scc *)
  let compute_timeout_time program appr scc =
    if Base.Set.exists ~f:(fun t -> Bound.is_infinity (Approximation.timebound appr t)) scc then
      0.
    else
      let toplogic_later_trans =
        program |> Program.transitions |> flip Base.Set.diff scc
        |> Base.Set.filter ~f:(fun t -> Bound.is_infinity (Approximation.timebound appr t))
      in
      !time_cfr
      *. float_of_int (Base.Set.length scc)
      /. float_of_int (Base.Set.length toplogic_later_trans + Base.Set.length scc)


  let merge_appr (program : Program.t) (program_cfr : Program.t) appr =
    let unchanged_trans = Set.inter (Program.transitions program) (Program.transitions program_cfr) in
    let appr_cfr = Approximation.empty |> TrivialTimeBounds.compute program_cfr in
    unchanged_trans
    |> Set.fold
         ~f:(fun appr_cfr trans ->
           let timebound = Approximation.timebound appr trans
           and costbound = Approximation.costbound appr trans in
           appr_cfr
           |> Approximation.add_timebound timebound trans
           |> Approximation.add_costbound costbound trans)
         ~init:appr_cfr


  let lift_to_program transform program =
    MaybeChanged.(
      transform (Program.graph program) >>= fun graph -> same (Program.map_graph (fun _ -> graph) program))
end
