open Batteries
open Bounds
open Polynomials
open PolyExponential
open ProgramModules
module Check_TWN = Check_TWN.Make (ProgramModules)
module Loop = Loop.Make (ProgramModules)
module SimpleCycle = SimpleCycle.Make (ProgramModules)

let heuristic_for_cycle appr program loop = Check_TWN.check_twn loop

module VT = struct
  type t = Transition.t * Var.t

  let equal (t1, v1) (t2, v2) = Transition.equal t1 t2 && Var.equal v1 v2
  let hash = Hashtbl.hash
end

module SizeBoundTable = Hashtbl.Make (VT)

let size_bound_table : (Transition.t * Bound.t) list option SizeBoundTable.t = SizeBoundTable.create 10

(** Internal memoization: The idea is to use this cache if we applied cfr and
  1) delete it and use the original cache if we get a timeout or
  2) if the analysis of the unrolled scc is completed successfully use this cache as the main memory.
  TODO Currently, we just reset the cache. *)
let reset_cfr () = SizeBoundTable.clear size_bound_table

let lift twn_proofs appr t var = function
  | None -> Bound.infinity
  | Some xs ->
      xs
      |> List.map (fun (entry, local_size) ->
             Bound.substitute_f (Approximation.sizebound appr entry) local_size)
      |> OurBase.Sequence.of_list |> Bound.sum
      |> tap (fun b ->
             ProofOutput.LocalProofOutput.add_to_proof twn_proofs
               FormattedString.(
                 fun () ->
                   mk_str_header_small @@ "TWN Size Bound - Lifting for " ^ Transition.to_id_string_pretty t
                   ^ " and " ^ Var.to_string ~pretty:true var ^ ": " ^ Bound.to_string ~pretty:true b))


module TWN_Complexity = TWN_Complexity.Make (ProgramModules)

let compute_time_bound twn_proofs loop =
  let mprf_bound = MultiphaseRankingFunction.time_bound loop 5 in
  if Bound.is_infinity mprf_bound && Check_TWN.check_twn loop then
    TWN_Complexity.complexity twn_proofs ~termination:false
      loop (* We assume that loop terminates (since t terminates). *)
  else
    mprf_bound


let improve_t program trans t appr =
  let twn_proofs = ProofOutput.LocalProofOutput.create () in
  Base.Set.fold
    ~f:(fun appr var ->
      if Polynomial.is_linear (TransitionLabel.update (Transition.label t) var |? Polynomial.of_var var) then
        appr
      else if
        SizeBoundTable.mem size_bound_table (t, var)
        && (not @@ Bound.is_polynomial @@ Approximation.sizebound appr t var)
      then (
        let lifted_bound = lift twn_proofs appr t var (SizeBoundTable.find size_bound_table (t, var)) in
        ProofOutput.add_to_proof_with_format (ProofOutput.LocalProofOutput.get_proof twn_proofs);
        Approximation.add_sizebound lifted_bound t var appr)
      else if not @@ Bound.is_polynomial @@ Approximation.sizebound appr t var then (
        ProofOutput.LocalProofOutput.add_to_proof twn_proofs (fun () ->
            FormattedString.mk_str_header_big @@ "TWN. Size Bound: " ^ Transition.to_id_string_pretty t
            ^ " for " ^ Var.to_string ~pretty:true var);
        let loops_opt =
          SimpleCycle.find_loop twn_proofs heuristic_for_cycle
            ~relevant_vars:(Option.some @@ VarSet.singleton var)
            appr program trans t
        in
        if Option.is_some loops_opt then (
          let loop, entries_traversal = Option.get loops_opt in
          let local_bound =
            let loop_red =
              Loop.eliminate_non_contributors ~relevant_vars:(Option.some @@ VarSet.singleton var) loop
            in
            (* We first compute for var (with a closed form) a local size bound *)
            let order = Check_TWN.check_triangular loop_red in
            if List.is_empty order then
              None
            else
              let closed_form =
                PE.compute_closed_form @@ List.map (fun var -> (var, Loop.update_var loop_red var)) order
                |> List.combine order
                |> List.find (Var.equal var % Tuple2.first)
                |> Tuple2.second
              in
              let time_bound = compute_time_bound twn_proofs loop in
              ProofOutput.LocalProofOutput.add_to_proof twn_proofs
                FormattedString.(
                  fun () ->
                    mk_str_line @@ "loop: " ^ Loop.to_string loop_red
                    <> mk_str_line @@ "closed-form: " ^ PE.to_string_pretty closed_form
                    <> mk_str_line @@ "runtime bound: " ^ Bound.to_string ~pretty:true time_bound);
              List.map
                (fun (entry, traversal) ->
                  ( entry,
                    PE.overapprox closed_form time_bound
                    |> Bound.substitute_f (fun var ->
                           Bound.of_poly @@ (Base.Map.find traversal var |? Polynomial.of_var var)) ))
                entries_traversal
              |> Option.some
          in
          SizeBoundTable.add size_bound_table (t, var) local_bound;
          (* Lifting previously computed local size bounds and store them in appr. *)
          let lifted_bound = lift twn_proofs appr t var local_bound in
          ProofOutput.add_to_proof_with_format (ProofOutput.LocalProofOutput.get_proof twn_proofs);
          Approximation.add_sizebound lifted_bound t var appr)
        else
          appr)
      else
        appr)
    (TransitionLabel.input_vars (Transition.label t))
    ~init:appr


let improve program ?(scc = None) appr =
  let trans = scc |? Base.Set.filter ~f:(Approximation.is_time_bounded appr) @@ Program.transitions program in
  Base.Set.fold ~f:(flip @@ improve_t program trans) trans ~init:appr
