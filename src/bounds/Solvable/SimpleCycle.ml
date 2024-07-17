open Batteries
open Formulas
open Polynomials

module Make (Bound : BoundType.Bound) (PM : ProgramTypes.ClassicalProgramModules) = struct
  open PM
  module Loop = Loop.Make (Bound) (PM)
  module TWN_Proofs = TWN_Proofs.Make (PM)
  module Approximation = Approximation.MakeForClassicalAnalysis (Bound) (PM)

  type path = (Location.t * TransitionLabel.t list * Location.t) list
  (** A simple cycle l0 ->_{t_11 || ... || t_1m} ... ->_{t_n1 || ... || t_nk} ln with
    - p.w.d locations,
    - no temp vars, and
    - we always consider a list of transitions with the same update. *)

  let _to_string path =
    List.map
      (fun (l, ts, l') ->
        "(" ^ Location.to_string l ^ ","
        ^ List.fold_right (fun t str -> str ^ TransitionLabel.to_string ~pretty:true t ^ " ") ts ""
        ^ "," ^ Location.to_string l' ^ ")")
      path
    |> String.concat " -> "


  let handled_transitions t =
    List.flatten (List.map (fun (l, ts, l') -> List.fold_right (fun t res -> (l, t, l') :: res) ts []) t)


  (* Computes all simple cycles using transitions from {trans}. *)
  let rec compute_cycles trans (paths : path list) res =
    (* Separate already completed cycles from "open" paths. *)
    let cycles, paths =
      List.partition
        (fun path ->
          let l_start = List.first path |> Tuple3.first and l_end = List.last path |> Tuple3.third in
          Location.equal l_start l_end)
        paths
    in
    if List.is_empty paths then
      cycles @ res
      (* Extend "open" paths. *)
    else
      let extended_paths =
        List.fold
          (fun paths path ->
            let l_end = List.last path |> Tuple3.third in
            let successors =
              List.filter
                (fun (l, _, l') ->
                  Location.equal l_end l
                  (* successor transition *)
                  && (not @@ List.exists (Location.equal l') (List.map Tuple3.third path))
                  (* does not form a lasso, i.e., l' is either not occuring in the path or equals l_start *))
                trans
            in
            let extend_path = List.map (fun t -> List.append path [ t ]) successors in
            extend_path @ paths)
          [] paths
      in
      compute_cycles trans extended_paths cycles @ res


  (** Computes all simple cycles which use t. *)
  let cycles_with_t trans t = compute_cycles trans [ [ t ] ] []

  (* determines all variables that get assigned a tmp value in a loop and removes them from the guard*)
  let update_path vars tmp_vars path =
    let varlist = Base.Set.to_list vars in
    let labels = List.map (List.first % Tuple3.second) path in
    (* HashSet where all elements are definitely not static*)
    let static_dep_table = Hashtbl.create 10 in
    Base.Set.iter ~f:(flip (Hashtbl.add static_dep_table) ()) tmp_vars;
    let non_static_vars =
      let maybe_changed non_statics =
        let update_is_static u =
          Base.Set.for_all ~f:(Option.is_none % Hashtbl.find_option static_dep_table) (UpdateElement.vars u)
        in
        (* all updates of var in the path *)
        let updates var = OurBase.List.filter_opt @@ List.map (flip TransitionLabel.update var) labels in
        let static_update var = List.for_all update_is_static @@ updates var in
        let new_non_static var = not (static_update var || Hashtbl.mem static_dep_table var) in
        List.find_opt new_non_static varlist
        |> Option.map_default
             (* If a var was found we add it to the table and the set*)
               (fun var ->
               Hashtbl.add static_dep_table var ();
               MaybeChanged.changed (Base.Set.add non_statics var))
             (MaybeChanged.same non_statics)
      in
      Util.find_fixpoint maybe_changed tmp_vars
    in
    List.map (Tuple3.map2 @@ List.map (TransitionLabel.relax_guard ~non_static:non_static_vars)) path


  let logger = Logging.(get Twn)

  (* We contract a (shifted to start) cycle to a loop  *)
  let contract_cycle (cycle : path) start =
    let pre, post = List.span (fun (l, _, _) -> not @@ Location.equal start l) cycle in
    let merge (_, ts, _) =
      ( List.map (Formula.mk % TransitionLabel.guard_without_inv) ts |> Formula.any,
        List.first ts |> TransitionLabel.update_map )
    in
    let merge_pre = List.map merge pre and merge_post = List.map merge post in
    List.fold Loop.append (List.first merge_post) (List.drop 1 merge_post @ merge_pre)


  (** This method computes a loop for every entry transition of the cycle.
    Notice that we do not regard costs in the chaining step.
    However, we consider them when we compute the final bound. *)
  let chain_cycle ?(relevant_vars = None) cycle program =
    let entries = Program.entry_transitions_with_logger logger program (handled_transitions cycle) in
    List.map
      (fun entry ->
        (entry, contract_cycle cycle (Tuple3.third entry) |> Loop.eliminate_non_contributors ~relevant_vars))
      entries


  type loop = Transition.t list * (Transition.t * Loop.t) list

  (** This function is used to obtain a set of loops which corresponds to simple cycles for corresponding entries. Used for TWN_Complexity. *)
  let find_all_loops twn_proofs ?(relevant_vars = None) choose_circle program scc (l, t, l') :
      loop ProofOutput.LocalProofOutput.with_proof List.t =
    let updated_trans = TransitionLabel.relax_guard ~non_static:VarSet.empty t in
    let handle_scc = List.map (Tuple3.map2 (TransitionLabel.relax_guard ~non_static:VarSet.empty)) in
    let merged_trans =
      Util.group
        (fun (l1, t, l1') (l2, t', l2') ->
          Location.equal l1 l2 && Location.equal l1' l2' && TransitionLabel.equivalent_update t t')
        (Base.Set.to_list scc |> handle_scc)
      |> List.map (fun xs ->
             ((Tuple3.first % List.first) xs, List.map Tuple3.second xs, (Tuple3.third % List.first) xs))
    in
    let merged_t =
      List.find_opt
        (fun (l1, ts, l1') ->
          List.exists (fun t1 -> TransitionLabel.equal updated_trans t1) ts
          && Location.equal l l1 && Location.equal l' l1')
        merged_trans
    in
    let cycles = cycles_with_t merged_trans @@ Option.get merged_t in
    let tmp_vars = Base.Set.diff (Program.vars program) (Program.input_vars program) in
    let handle_cycles = List.map @@ update_path (Program.input_vars program) tmp_vars in
    List.filter_map (fun cycle ->
        let twn_proofs = ProofOutput.LocalProofOutput.copy twn_proofs in
        let chained_cycle = chain_cycle ~relevant_vars cycle program in
        if List.for_all (fun (entry, loop) -> choose_circle loop entry) chained_cycle then (
          let handled_transitions = handled_transitions cycle in
          TWN_Proofs.add_to_proof_graph twn_proofs program handled_transitions
            (Program.entry_transitions_with_logger logger program handled_transitions);
          let loop : loop = (handled_transitions, chained_cycle) in
          Some ProofOutput.LocalProofOutput.{ result = loop; proof = twn_proofs })
        else
          None)
    @@ handle_cycles cycles


  (** Computes update_n * ... * update_i where update_n is the update of a transition (_,_,target) and resp. update_i for (start,_,_). *)
  let traverse_cycle (cycle : path) start target =
    if Location.equal start target then
      TransitionLabel.update_map @@ List.first @@ Tuple3.second (List.first cycle)
    else
      let target_t = List.find (Location.equal target % Tuple3.third) cycle in
      let traversal =
        cycle @ cycle
        |> List.drop_while (Location.equal start % Tuple3.first)
        |> List.take_while (not % Location.equal target % Tuple3.third)
        |> List.rev |> ( @ ) [ target_t ]
      in
      let substitution update_map var = Base.Map.find update_map var |? Polynomial.of_var var in
      List.fold
        (fun map (_, ts, _) ->
          Base.Map.map
            ~f:(Polynomial.substitute_f (substitution (TransitionLabel.update_map @@ List.first ts)))
            map)
        (Base.Map.empty (module Var))
        traversal


  (** This function is used to obtain a loop which corresponds to a simple cycle. Used for SizeBounds. *)
  let find_loop twn_proofs ?(relevant_vars = None) f appr program scc (l, t, l') =
    if not @@ TransitionLabel.has_tmp_vars t then
      let merged_trans =
        Util.group
          (fun (l1, t, l1') (l2, t', l2') ->
            Location.equal l1 l2 && Location.equal l1' l2' && TransitionLabel.equivalent_update t t')
          (Base.Set.to_list scc |> List.filter (not % TransitionLabel.has_tmp_vars % Tuple3.second))
        |> List.map (fun xs ->
               ((Tuple3.first % List.first) xs, List.map Tuple3.second xs, (Tuple3.third % List.first) xs))
      in
      let merged_t =
        List.find
          (fun (l1, ts, l1') ->
            List.exists (fun t1 -> TransitionLabel.equal t t1) ts
            && Location.equal l l1 && Location.equal l' l1')
          merged_trans
      in
      let cycles = cycles_with_t merged_trans merged_t in
      List.find_map_opt
        (fun cycle ->
          let loop = contract_cycle cycle l in
          let loop_red = Loop.eliminate_non_contributors ~relevant_vars loop in
          if f appr program loop_red then (
            let handled_transitions = handled_transitions cycle in
            TWN_Proofs.add_to_proof_graph twn_proofs program handled_transitions
              (Program.entry_transitions_with_logger logger program handled_transitions);
            Option.some
              ( loop,
                List.map
                  (fun entry -> (entry, traverse_cycle cycle (Transition.src entry) l))
                  (Program.entry_transitions_with_logger logger program handled_transitions) ))
          else
            None)
        cycles
    else
      None
end
