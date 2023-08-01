open Batteries
open ProgramModules
open FormattedString

(* PROOF *)

let proof = ref Empty

let get_proof () = let p = !proof in proof := Empty; p

(* let add_to_proof () =
  ProofOutput.add_to_proof (fun () -> !proof);
  proof := Empty *)

let proof_append f_str =
  proof :=  !proof <> f_str

let proof_reset () = proof := Empty

module Make(PM: ProgramTypes.ClassicalProgramModules) = struct
  open PM

  module GraphPrint = GraphPrint.Make(PM)

  let add_to_proof_graph program cycle entries =
    let color_map =
    List.fold_right (fun t -> OurBase.Map.add_or_overwrite ~key:t ~data:GraphPrint.Blue) cycle GraphPrint.TransitionMap.empty
    |> fun m -> List.fold_right (fun t -> OurBase.Map.add_or_overwrite ~key:t ~data:GraphPrint.Red) entries m in
      proof_append @@ mk_paragraph (
        match ProofOutput.get_format () with
          | Html -> mk_raw_str (GraphPrint.print_system_pretty_html color_map program)
          | _    -> Empty);
    proof_append @@ mk_str_line @@ "  cycle: " ^ (Util.enum_to_string Transition.to_id_string_pretty @@ List.enum cycle)
end
