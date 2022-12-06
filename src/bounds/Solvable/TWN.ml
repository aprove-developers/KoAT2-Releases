open Batteries
open Polynomials
open Formulas
open Atoms
open BoundsInst
open Constraints
open PolyExponential
open Automorphism

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

  (* CYLES and corresponding time-bound. *)

  type path = (Location.t * TWNLoop.t * Location.t) list

  (* Computes all cycles containing l0. The function call "cycles trans l0 [l0 ->_t l1] []" returns all paths containing t *)
  let rec cycles trans l0 (paths: (path * LocationSet.t) list) (res: path list) =
    if List.is_empty paths then res
    else
      let (path, loc_done) = List.first paths in
      let path_trans = Set.filter
        (fun (l,_,l') -> let (l_path,_,l_path') = List.first path in Location.equal l l_path' && not (LocationSet.mem l' loc_done)) trans in
      let cycle_trans = Set.filter (fun (_,_,l') -> Location.equal l' l0) path_trans in
      cycles trans l0
        ((List.tl paths) @ (List.map (fun (l,t,l') -> ((l,t,l')::path, LocationSet.add l loc_done)) ((Set.diff path_trans cycle_trans) |> Set.to_list)))
        res @ (List.map (fun t -> (t::path)|> List.rev) (Set.to_list cycle_trans))

  (* We compute the transitions l->l by contracting a (shifted to start) cycle (l1 ->_t1 l2 ->_t2 ... -> ln ->_tn l1 with the update tn % ... % t_1
  and the guard g1 && g1(t1) && g2(t2 % t1) && .. and cost)  *)
  let compose_transitions cycle start =
    let rec split i = function
      | [] -> raise Not_found
      | x::xs ->
          if i == 0 then ([],x,xs) else let y,t,z = split (i - 1) xs in (x::y,t, z)
    in
    let pre, t1, post = split start cycle in
    List.fold TWNLoop.append t1 (post@pre)

  let corresponding_transition t (trans: TransitionSet.t) = List.find (fun (_,t',_) -> (TransitionLabel.id t) == (TransitionLabel.id t')) (trans |> TransitionSet.to_list)

  (* Finds entered location on cycle. *)
  let rec find l scc list =
      match list with
      | [] ->  raise Not_found
      | t::xs -> let (_,_,l') = corresponding_transition t scc in if Location.equal l l' then 0 else 1 + (find l scc xs)

  (** lift_option [[Some x; Some y]] returns [Some [x;y]]. Returns none if none is element of list. *)
  let lift_option (xs:'a option list) : 'a list option =
    if List.for_all Option.is_some xs then
      Some (List.map Option.get xs)
    else
      None

  exception No_Cycle

  let check_non_increasing (twn_loop: TWNLoop.t) t =
    List.for_all (fun atom ->
      (** Here, an atom has the form poly < 0. *)
      let poly = Atom.poly atom in
      let poly_updated = Polynomial.substitute_f (fun v -> TransitionLabel.update t v |? Polynomial.of_var v) poly in
      let atom = Atom.mk_le poly poly_updated |> Atom.neg
      and guard = TransitionLabel.guard t in
      SMTSolver.tautology Formula.(implies (mk guard) (mk [atom])))
      ( TWNLoop.guard_without_inv twn_loop
        |> Formula.atoms
        |> List.filter (TWN_Termination.check_update_invariant twn_loop) )

  let find_non_increasing_set program appr decreasing loop entry =
    let rec f non_increasing entries =
      if List.for_all (fun entry -> (Approximation.is_time_bounded appr entry) && (Approximation.is_size_bounded program appr entry)) entries then
        Option.some (non_increasing, entries)
      else
        let bounded, unbounded = List.partition (fun entry -> Approximation.is_time_bounded appr entry && Approximation.is_size_bounded program appr entry) entries in
        if List.for_all (check_non_increasing loop) (List.map Tuple3.second unbounded) then
          let new_entries =
            bounded @ (Program.entry_transitions logger program (unbounded @ non_increasing))
          in
          f (unbounded @ non_increasing) new_entries
        else
          None in
    f (List.map (fun t -> corresponding_transition t (Program.transitions program)) decreasing) [entry]

  let find_cycle appr scc program (cycles: path list) transformation_type =
    try
    List.find_map (fun cycle ->
      (* list of all transitions in a cycle, twnloop doesn't require each transition to be in twn form, but together they do *)
        let handled_transitions = List.fold (fun xs (l,twn,l') -> (List.map (fun t -> (l,t,l')) (TWNLoop.subsumed_transitionlabels twn))@xs) [] cycle in
        let entries = Program.entry_transitions logger program handled_transitions in
        let twn_loops = List.map (fun (_,_,l') -> compose_transitions (cycle |> List.map Tuple3.second) (find l' scc (cycle |> List.map (List.first % TWNLoop.subsumed_transitionlabels % Tuple3.second)))) entries in (* 'find' throws an exception *)
        let loop_entry = (List.combine twn_loops entries) in
        if (List.for_all (fun (t, entry) ->
            let eliminated_t = (* throw out useless variables *)
              EliminateNonContributors.eliminate_t
                (TWNLoop.input_vars t) (Formula.vars @@ TWNLoop.guard t) (TWNLoop.update t) (TWNLoop.remove_non_contributors t)
            in
            not (VarSet.is_empty (TWNLoop.vars eliminated_t)) (* Are there any variables *)
            && VarSet.equal (TWNLoop.vars eliminated_t) (TWNLoop.input_vars eliminated_t) (* No Temp Vars? *))
          loop_entry
        ) then (
          let subsumed_transitionlabels = List.map (TWNLoop.subsumed_transitionlabels % Tuple3.second) cycle |> List.fold (@) [] in
          let non_increasing_opt = List.map (uncurry @@ (find_non_increasing_set program appr subsumed_transitionlabels)) loop_entry in
          if List.for_all Option.is_some non_increasing_opt then
            lift_option @@ List.map (handle_transformation transformation_type)
              ( List.combine (List.map Option.get non_increasing_opt) twn_loops
                |> List.combine entries |> List.map (fun (x,(y,z)) -> (x,y,z)) ) (* tries to transform into loops. *)
          else
            None)
        else
          None  (*Not twn nor twn-transformable *)
      )
      cycles
      with
      | Not_found -> raise No_Cycle

  (** Gets a list of transitions and rec. merges them into twn-loops, i.e., disjunctions of transitions. *)
  let rec parallel_edges ys = function
    | [] -> ys
    | (l,t,l')::xs -> let f (l1,loop,l1') = Location.equal l l1 && Location.equal l' l1' && String.equal (TransitionLabel.update_to_string_rhs t) (TWNLoop.update_to_string_rhs loop) in
                      let h (l1,t1,l1') = Location.equal l l1 && Location.equal l' l1' && String.equal (TransitionLabel.update_to_string_rhs t) (TransitionLabel.update_to_string_rhs t1) in
    if List.exists f ys then parallel_edges ys xs
    else parallel_edges ((l, TWNLoop.mk_transitions List.(map Tuple3.second (filter h ((l,t,l')::xs))), l')::ys) xs

  module TimeBoundTable = Hashtbl.Make(Transition)

  (* keys: liste von transition (kreise), values: bounds von eingangstransitionen *)
  let time_bound_table: ((Transition.t list) * (Transition.t list * Bound.t) list) TimeBoundTable.t = TimeBoundTable.create 10

  let lift appr entries bound =
    List.map (fun entry ->
    let bound_with_sizebound = Bound.substitute_f (Approximation.sizebound appr entry) bound in
      Bound.mul (Approximation.timebound appr entry) bound_with_sizebound
      |> tap (fun b ->
      TWN_Proofs.proof_append (FormattedString.(mk_paragraph (mk_str_line ("relevant size-bounds w.r.t. t" ^ (Transition.id entry |> Util.natural_to_subscript) ^ ":") <> (
            Bound.vars bound
            |> VarSet.to_list
            |> List.map (fun v -> (Var.to_string ~pretty:true v) ^ ": " ^ (Approximation.sizebound appr entry v |> Bound.to_string ~pretty:true))
            |> List.map (FormattedString.mk_str_line) |> FormattedString.mappend) <>
            FormattedString.mk_str_line ("Runtime-bound of t" ^ (Transition.id entry |> Util.natural_to_subscript) ^ ": " ^ (Approximation.timebound appr entry |> Bound.to_string ~pretty:true)) <>
            FormattedString.mk_str ("Results in: " ^ (Bound.to_string ~pretty:true b))))))) entries
    |> Bound.sum_list

  let check_update_invariant twn_loop atom =
    let poly = Atom.poly atom in
    let poly_updated = Polynomial.substitute_f (TWNLoop.update_full twn_loop) poly in
    let atom_updated = Atom.mk_le poly_updated Polynomial.zero in
    SMTSolver.tautology Formula.(implies (mk [atom]) (mk [atom_updated]))

  let time_bound (l,transition,l') scc program appr transformation_type =
    TWN_Proofs.proof := FormattedString.Empty;
    let opt = TimeBoundTable.find_option time_bound_table (l,transition,l') in
    let bound =
    if Option.is_none opt then (
      let bound =
        Timeout.timed_run 5. (fun () -> try
          let parallel_edges = parallel_edges [] (TransitionSet.to_list scc) in
          let x =
            if Location.equal l l' then
              let f (l1,loop,l1') = Location.equal l l1 && Location.equal l' l1' && ProgramTypes.VarMap.equal Polynomial.equal (TransitionLabel.update_map transition) (TWNLoop.update_map loop) in
              [[List.find f parallel_edges]]
            else
              (cycles (parallel_edges |> List.filter (fun (l,_,l') -> not (Location.equal l l')) |> Set.of_list) l ([([(l,(TWNLoop.mk_transition transition),l')], (LocationSet.singleton l'))]) []) in
          let (entries_org, non_incr_entries, cycle, automorphisms) =
            let cycles = find_cycle appr scc program x transformation_type in
            (* find_cycle throws an exception if no cycle is found (for efficiency reasons) *)
            List.fold_right (fun (x0,x1,x2,x3) (xs0,xs1,xs2,xs3) -> ((x0::xs0),(x1::xs1),(x2::xs2),(x3::xs3))) cycles ([],[],[],[])
          in
          let handled_transitions =
            ListMonad.(cycle >>= fun loop -> TWNLoop.subsumed_transitions scc loop)
          in
          let twn_loops =
            List.map (fun (l,t,l') -> compose_transitions cycle (find l' scc (cycle |> List.map (List.first % TWNLoop.subsumed_transitionlabels)))) entries_org
          in
          TWN_Proofs.proof_append FormattedString.((mk_header_small (mk_str "Cycles:")) <>
            (List.combine (List.map Transition.to_string_pretty entries_org) (List.map (TWNLoop.to_string ~pretty:true) twn_loops)
            |> List.map (fun (a,b) -> FormattedString.mk_str_line ("entry: " ^ a) <> FormattedString.mk_block (FormattedString.mk_str_line ("results in twn-loop: " ^ b)))
            |> FormattedString.mappend));
          let global_local_bounds =
            List.map (fun (entry_org, (non_increasing,new_entries), loop, automorphism) -> (* entry,twn,automorphism *)
              TWN_Proofs.proof_append FormattedString.(mk_header_small (mk_str @@ "Cycle by " ^  (Transition.to_id_string_pretty entry_org)));
              let program_with_one_entry =
                let non_entries =
                  TransitionSet.to_list (TransitionSet.diff (Program.transitions program) (TransitionSet.of_list entries_org))
                in
                Program.from_enum
                  (Program.start program)
                  (List.enum non_entries (* TODO simplify *)
                  |> Enum.append (Enum.singleton entry_org))

              in
              let twn_inv =
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
              Bound.infinity)
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
      bound
end
