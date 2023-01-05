open Automorphism
open Atoms
open Batteries
open BoundsInst
open Constraints
open Formulas
open PolyExponential
open Polynomials

let logger = Logging.(get Twn)

module SMTSolver = SMT.Z3Solver
module SMTSolverTimeout = SMT.Z3SolverTimeout

type ('twnt,'tt) twn_transformation_fun_type =
  'tt * ('tt list * 'tt list) * 'twnt -> ('tt * ('tt list * 'tt list) * 'twnt * Automorphism.t) option

type twn_transformation_fun_type_transformable =
  (TWNLoop.Make(ProgramModules).t, ProgramModules.Transition.t) twn_transformation_fun_type

type _ configuration = NoTransformation: ('a,'b) twn_transformation_fun_type configuration
                     | Transformation: Transformation.transformation_type
                                     -> (TWNLoop.Make(ProgramModules).t,
                                         ProgramModules.Transition.t) twn_transformation_fun_type configuration

(** Applies any required transformation *)
let handle_transformation (type a) (conf: a configuration): a =
  match conf with | Transformation transformation -> Transformation.transform transformation
                  | NoTransformation -> fun (a,b,c) -> Some (a,b,c,Automorphism.identity_aut)

module Make(PM: ProgramTypes.ClassicalProgramModules) = struct
  open PM

  module Approximation = Approximation.MakeForClassicalAnalysis(PM)
  module InvariantGeneration = InvariantGeneration.Make(PM)
  module TWNLoop = TWNLoop.Make(PM)
  module TWN_Complexity = TWN_Complexity.Make(PM)
  module TWN_Termination = TWN_Termination.Make(PM)
  module Check_TWN = Check_TWN.Make(PM)
  module SimpleCycle = SimpleCycle.SimpleCycle(PM)

  module TimeBoundTable = Hashtbl.Make(Transition)

  (* Keys: transition, values: bounds of entry transitions. *)
  let time_bound_table: (Transition.t * Bound.t) list TimeBoundTable.t = TimeBoundTable.create 10

  let lift appr entry bound =
    let bound_with_sizebound = Bound.substitute_f (Approximation.sizebound appr entry) bound in
      Bound.mul (Approximation.timebound appr entry) bound_with_sizebound
      |> tap @@ fun b ->
      TWN_Proofs.proof_append @@ FormattedString.(mk_paragraph (mk_str_line ("relevant size-bounds w.r.t. t" ^ (Transition.id entry |> Util.natural_to_subscript) ^ ":") <> (
            Bound.vars bound
            |> VarSet.to_list
            |> List.map (fun v -> (Var.to_string ~pretty:true v) ^ ": " ^ (Approximation.sizebound appr entry v |> Bound.to_string ~pretty:true))
            |> List.map (FormattedString.mk_str_line) |> FormattedString.mappend) <>
            FormattedString.mk_str_line ("Runtime-bound of t" ^ (Transition.id entry |> Util.natural_to_subscript) ^ ": " ^ (Approximation.timebound appr entry |> Bound.to_string ~pretty:true)) <>
            FormattedString.mk_str ("Results in: " ^ (Bound.to_string ~pretty:true b))))

  let heuristic_for_cycle appr entry loop =
    Check_TWN.check_twn_loop loop && Approximation.is_time_bounded appr entry

  let time_bound (l,t,l') scc program appr transformation_type =
    TWN_Proofs.proof := FormattedString.Empty;
    let opt = TimeBoundTable.find_option time_bound_table (l,t,l') in
    if Option.is_none opt then (
      let bound = Timeout.timed_run 5. (fun () ->
      (* We have not yet computed a (local) runtime bound. *)
      let loops_opt = SimpleCycle.find_loops heuristic_for_cycle appr program scc t in
      if Option.is_some loops_opt then
        List.map (fun (entry,loop) ->
          TWN_Complexity.complexity loop |> lift appr entry)
        (Option.get loops_opt)
        |> List.enum
        |> Bound.sum
      else
        Bound.infinity) in
      if Option.is_some bound then
        Tuple2.first @@ Option.get bound
      else
        Bound.infinity
    ) else (
      (* We already have computed a (local) runtime bound and just lift it again.*)
      let xs = Option.get opt in
      let bound_with_sizebound = Bound.sum_list (List.map (fun (entry, bound) -> lift appr entry bound) xs) in
      bound_with_sizebound
    )

    (* TWN_Proofs.proof := FormattedString.Empty;
    let opt = TimeBoundTable.find_option time_bound_table (l,t,l') in
    let bound =
    if Option.is_none opt then (
      let bound =
        Timeout.timed_run 5. (fun () -> try
          let loops = SimpleCycle.SimpleCycle(ProgramModules).find_cycle appr program scc t;
          let entries = [] in (* TODO *)

          (* TWN_Proofs.proof_append FormattedString.((mk_header_small (mk_str "Cycles:")) <>
            (List.combine (List.map Transition.to_string_pretty entries_org) (List.map (TWNLoop.to_string ~pretty:true) twn_loops)
            |> List.map (fun (a,b) -> FormattedString.mk_str_line ("entry: " ^ a) <> FormattedString.mk_block (FormattedString.mk_str_line ("results in twn-loop: " ^ b)))
            |> FormattedString.mappend)); *)
          let global_local_bounds =
              List.map (fun (entry, loop) -> (* TODO extend for automorphisms (entry, loop, automorphism) *)
              (* TWN_Proofs.proof_append FormattedString.(mk_header_small (mk_str @@ "Cycle by " ^  (Transition.to_id_string_pretty entry_org))); *)
              let program_with_one_entry =
                let non_entries =
                  TransitionSet.to_list (TransitionSet.diff (Program.transitions program) (TransitionSet.of_list entries))
                in
                Program.from_enum
                  (Program.start program)
                  (List.enum non_entries (* TODO simplify *)
                  |> Enum.append (Enum.singleton entry))
              in

              (* let twn_inv =
                program_with_one_entry
                |> InvariantGeneration.transform_program
                |> MaybeChanged.unpack
                |> Program.transitions
                |> TransitionSet.filter (fun t -> List.exists (Transition.equal t) handled_transitions)
                |> TransitionSet.to_list
                |> List.map (TransitionLabel.invariant % Transition.label)
                |> fun invariants -> List.flatten invariants |> List.filter (fun atom -> List.for_all (List.exists (Atom.equal atom)) invariants)
                |> TWNLoop.add_invariant loop in
              let eliminated_t = EliminateNonContributors.eliminate_t
                  (TWNLoop.input_vars twn_inv) (Formula.vars @@ TWNLoop.guard twn_inv) (TWNLoop.update twn_inv) (TWNLoop.remove_non_contributors twn_inv)
              in
              if VarSet.is_empty (TWNLoop.vars eliminated_t) then
                Bound.infinity, ([entry_org], Bound.infinity)
              else (
                let bound = Automorphism.transform_bound automorphism @@ TWN_Complexity.complexity eliminated_t in
                if Bound.is_infinity bound then
                  raise (TWN_Termination.Non_Terminating (handled_transitions, [entry_org]));
                  TWN_Proofs.proof_append FormattedString.(
                    mk_header_small (mk_str @@ "Lift Bound: ")
                    <> mk_str_line @@ "Compute new entries: " ^ (new_entries |> List.enum |> Util.enum_to_string Transition.to_id_string_pretty)
                    <> mk_str_line @@ "Non increasing transitions: " ^ (non_increasing |> List.enum |> Util.enum_to_string Transition.to_id_string_pretty));
                  lift appr new_entries bound, (new_entries, bound)))
            (List.map2 (fun (a,b) (c,d) -> (a,b,c,d)) (List.combine entries_org non_incr_entries) (List.combine twn_loops automorphisms))
          in
          List.iter (fun t -> TimeBoundTable.add time_bound_table t (handled_transitions, List.map Tuple2.second global_local_bounds)) handled_transitions;
          global_local_bounds |> List.map Tuple2.first |> Bound.sum_list
          with
          | No_Cycle -> Logger.log logger Logger.DEBUG (fun () -> "twn", ["no twn_cycle found", ""]); Bound.infinity
          | TWN_Termination.Non_Terminating (handled_transitions,entries)->
              Logger.log logger Logger.DEBUG (fun () -> "twn", ["non terminating", ""]);
              List.iter (fun t -> TimeBoundTable.add time_bound_table t (handled_transitions, List.map (fun t -> ([t], Bound.infinity)) entries)) handled_transitions;
              Bound.infinity)   *)
      in
      if Option.is_some bound then
        bound |> Option.get |> Tuple2.first
      else (
        Logger.log logger Logger.INFO (fun () -> "twn", ["Timeout", Bound.to_string Bound.infinity]);
        Bound.infinity)
    )
    else (
      let cycle, xs = Option.get opt in
      let bound_with_sizebound = Bound.sum_list (List.map (fun (entry, bound) -> lift appr entry bound) xs) in
      bound_with_sizebound |> tap (fun b -> Logger.log logger Logger.INFO (fun () -> "twn", ["global_bound", Bound.to_string b]))
                          |> tap (fun b -> TWN_Proofs.proof_append FormattedString.((mk_str_line (b |> Bound.to_string ~pretty:true))))) in
      (if Bound.compare_asy bound (Approximation.timebound appr (l,transition,l')) < 0 then
        let formatted = FormattedString.((mk_header_big @@ mk_str "Time-Bound by TWN-Loops:") <>
                mk_header_small (mk_str ("TWN-Loop t" ^ (TransitionLabel.id transition |> Util.natural_to_subscript) ^ " with runtime bound " ^ Bound.to_string ~pretty:true bound)) <>
                !TWN_Proofs.proof) in
        ProofOutput.add_to_proof @@ fun () -> formatted);
      bound*)
end
