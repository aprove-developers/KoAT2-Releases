open! OurBase
open FormattedString

module Make (PM : ProgramTypes.ClassicalProgramModules) = struct
  open PM
  module GraphPrint = GraphPrint.MakeFromClassical (PM)

  let add_to_proof_graph (t : ProofOutput.LocalProofOutput.t) program cycle entries =
    let color_map =
      List.fold_right
        ~f:(fun t -> Map.set ~key:t ~data:GraphPrint.Blue)
        cycle ~init:GraphPrint.empty_color_map
      |> fun m -> List.fold_right ~f:(fun t -> Map.set ~key:t ~data:GraphPrint.Red) entries ~init:m
    in
    ProofOutput.LocalProofOutput.add_to_proof_with_format t (fun format ->
        mk_paragraph
          (match format with
          | Html -> mk_raw_str (GraphPrint.print_system_pretty_html ~color_map program)
          | _ -> Empty));
    ProofOutput.LocalProofOutput.add_to_proof t (fun () ->
        mk_str_line @@ "  cycle: "
        ^ Util.sequence_to_string ~f:Transition.to_id_string_pretty
        @@ Sequence.of_list cycle)
end
