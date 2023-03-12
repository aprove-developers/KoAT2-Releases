open Batteries
open ProgramTypes
open ProgramModules
open Polynomials
open PolyExponential
open BoundsInst
open Constraints

module Check_Solvable = Check_Solvable.Make(ProgramModules)
module Loop = Loop.Make(ProgramModules)
module SimpleCycle = SimpleCycle.Make(ProgramModules)

let matrix_of_linear_assignments loop (block: Var.t list) =
  (** get_linear_update_list (x<- 2x+3y+y^2) x [x;y] returns [2;3] *)
  let rec get_linear_update_of_var loop (block: Var.t list) (var_left: Var.t) = match block with
    | [] -> []
    | x::xs ->
      (Polynomial.coeff_of_indeterminate x (Loop.update_var loop var_left)) ::
      (get_linear_update_of_var loop xs var_left)
  in
  List.map (get_linear_update_of_var loop block) block

(* This function was written by Tom Küspert *)
let read_process_lines command =
  let lines = ref [] in
  let in_channel = Unix.open_process_in command in
  begin
    try
      while true do
        lines := input_line in_channel :: !lines
      done;
    with
      | BatInnerIO.Input_closed -> ()
      | End_of_file -> ()
  end;
  List.rev !lines

let run_python var block update_matrix =
  let vars_str = (* vars x1,x2,... are encoded (for the python script) by ["x1","x2",...]. *)
    block
    |> List.map (fun var -> "\"" ^ (Var.to_string var) ^ "\"")
    |> List.enum
    |> Util.enum_to_string identity
    |> Str.global_replace (Str.regexp ";") "," and
  update_str = (* update matrix A encoded (for the python script) row-wise w.r.t. x1,x2,.... *)
    update_matrix
    |> List.flatten
    |> List.enum
    |> Util.enum_to_string OurInt.to_string
    |> Str.global_replace (Str.regexp ";") "," in
  let command = "python3 -c 'from python.SizeBoundSolvable import size_bound; size_bound(" ^ update_str ^ "," ^ vars_str ^ ",\"" ^ Var.to_string var ^ "\")'" in
  let python_output = read_process_lines command in
  match python_output with
      | [error] -> None
      | [n;a] -> Some (int_of_string n,a |> Readers.read_bound)
      | _ -> None (* Error string *)

let heuristic_for_cycle appr program loop =
  Option.is_some @@ Check_Solvable.check_solvable loop
  && Base.Set.for_all ~f:(fun var -> Polynomial.is_linear @@ Loop.update_var loop var) (Loop.updated_vars loop)
  && Base.Set.for_all ~f:(fun var -> Polynomial.no_constant_addend @@ Loop.update_var loop var) (Loop.updated_vars loop)
  && Base.Set.length @@ Loop.vars loop > 1

module VT = struct
  type t = Transition.t * Var.t

  let equal (t1,v1) (t2,v2) = Transition.equal t1 t2 && Var.equal v1 v2

  let hash = Hashtbl.hash
end

module SizeBoundTable = Hashtbl.Make(VT)

let size_bound_table: (Transition.t * Bound.t) list option SizeBoundTable.t = SizeBoundTable.create 10

(** Internal memoization: The idea is to use this cache if we applied cfr and
  1) delete it and use the original cache if we get a timeout or
  2) if the analysis of the unrolled scc is completed successfully use this cache as the main memory.
  TODO Currently, we just reset the cache. *)
let reset_cfr () =
  SizeBoundTable.clear size_bound_table

let lift appr t var = function
  | None -> Bound.infinity
  | Some xs -> xs
               |> List.map (fun (entry,local_size) -> Bound.substitute_f (Approximation.sizebound appr entry) local_size)
               |> List.enum
               |> Bound.sum
               |> tap (fun b -> TWN_Proofs.proof_append FormattedString.(mk_str_header_small @@ "Solv. Size Bound - Lifting for " ^ (Transition.to_id_string_pretty t) ^ " and " ^ Var.to_string ~pretty:true var ^ ": " ^ Bound.to_string ~pretty:true b))


let improve_t program trans t appr =
  TWN_Proofs.proof_reset();
  Base.Set.fold ~f:(fun appr var ->
    if SizeBoundTable.mem size_bound_table (t, var) && not @@ Bound.is_polynomial @@ Approximation.sizebound appr t var then
        let lifted_bound = lift appr t var (SizeBoundTable.find size_bound_table (t,var)) in
        let proof = TWN_Proofs.get_proof () in
        ProofOutput.add_to_proof (fun () -> proof);
        Approximation.add_sizebound lifted_bound t var appr
    else
      if not @@ Bound.is_polynomial @@ Approximation.sizebound appr t var then (
        TWN_Proofs.proof_append @@ FormattedString.mk_str_header_big @@ "Solv. Size Bound: " ^ (Transition.to_id_string_pretty t) ^ " for " ^ Var.to_string ~pretty:true var;
        let loops_opt = SimpleCycle.find_loop ~relevant_vars:(Option.some @@ VarSet.singleton var) heuristic_for_cycle appr program trans t in
        if Option.is_some loops_opt then
          let loop, entries_traversal = Option.get loops_opt in
          let loop_red = Loop.eliminate_non_contributors ~relevant_vars:(Option.some @@ VarSet.singleton var) loop in
          let local_bound =
            (* We first compute for every var (with a closed form) and every entry a local size bound *)
              let blocks = Check_Solvable.check_solvable loop_red in
              if Option.is_none blocks then Bound.infinity
              else (
                (* As we obtain minimal solvable blocks, we have to merge them, e.g., if we consider Y in for (X,Y) <- (2*X,X), we infer the blocks [X],[Y] and have to merge them to [X,Y] . *)
                let block = List.flatten @@ List.filter (fun block -> List.exists (flip List.mem block) (Base.Set.to_list @@ (Base.Set.add (Loop.updated_vars loop_red) var))) @@ Option.get blocks in
                let opt = run_python var block (matrix_of_linear_assignments loop_red block) in
                if Option.is_some opt then
                  let (n, b) = Option.get opt in
                  Bound.add (Loop.compute_bound_n_iterations loop_red var n) b
                else
                  Bound.infinity)
          in
          let time_bound = MultiphaseRankingFunction.time_bound loop 5 in
          TWN_Proofs.proof_append FormattedString.(mk_str_line @@ "loop: " ^ Loop.to_string loop_red <> mk_str_line @@ "overappr. closed-form: " ^ Bound.to_string ~pretty:true local_bound <> mk_str_line @@ "runtime bound: " ^ Bound.to_string ~pretty:true time_bound);
          let res = List.map (fun (entry,traversal) -> entry,
            Bound.substitute (Var.of_string "n") ~replacement:time_bound local_bound
            |> Bound.substitute_f (fun var -> Bound.of_poly @@ (Base.Map.find traversal var |? Polynomial.of_var var))
          ) entries_traversal |> Option.some
          in
          SizeBoundTable.add size_bound_table (t,var) res;
          (* Lifting previously computed local size bounds and store them in appr. *)
          let lifted_bound = lift appr t var res in
          let proof = TWN_Proofs.get_proof () in
          ProofOutput.add_to_proof (fun () -> proof);
          Approximation.add_sizebound lifted_bound t var appr
        else
          appr)
      else
        appr) (TransitionLabel.input_vars (Transition.label t)) ~init:appr

let improve program ?(scc = None) appr =
  let trans = scc |? Base.Set.filter ~f:(Approximation.is_time_bounded appr) @@ Program.transitions program in
  Base.Set.fold ~f:(flip @@ improve_t program trans) trans ~init:appr
