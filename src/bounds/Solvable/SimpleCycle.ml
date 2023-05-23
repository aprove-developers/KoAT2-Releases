open Atoms
open Batteries
open Constraints
open Formulas
open Polynomials
open Transformation

module VarMap = Map.Make(Var)
module Make(PM: ProgramTypes.ClassicalProgramModules) = struct
  open PM

  module Loop = Loop.Make(PM)
  module Transformation = Transformation.Make(PM)
  module Approximation = Approximation.MakeForClassicalAnalysis(PM)

  (** A simple cycle l0 ->_{t_11 || ... || t_1m} ... ->_{t_n1 || ... || t_nk} ln with
    - p.w.d locations,
    - no temp vars, and
    - we always consider a list of transitions with the same update. *)
  type path = (Location.t * TransitionLabel.t list * Location.t) list

  let _to_string path = List.map (fun (l,ts,l') -> "(" ^ Location.to_string l ^ "," ^ List.fold_right (fun t str -> str ^ TransitionLabel.to_string ~pretty:true t ^ " ") ts "" ^ "," ^ Location.to_string l' ^ ")") path |> String.concat " -> "

  let handled_transitions t =
    List.flatten (List.map (fun (l,ts,l') -> List.fold_right (fun t res -> (l,t,l')::res) ts []) t)

  (* Computes all simple cycles using transitions from {trans}. *)
  let rec compute_cycles trans (paths: path list) res =
    (* Separate already completed cycles from "open" paths. *)
    let (cycles, paths) =
      List.partition (fun path ->
        let
          l_start = List.last path |> Tuple3.first and
          l_end = List.first path |> Tuple3.third in
        Location.equal l_start l_end
      )
      paths
    in
    if List.is_empty paths then (List.map List.rev cycles@res)
    (* Extend "open" paths. *)
    else
      let extended_paths =
        List.fold (fun paths path ->
        let l_end = List.first path |> Tuple3.third in
        let successors = List.filter (
          fun (l,_,l') ->
            Location.equal l_end l (* successor transition *) &&
            not @@ List.exists (Location.equal l') (List.map Tuple3.third path) (* does not form a lasso, i.e., l' is either not occuring in the path or equals l_start *)
        ) trans in
        let extend_path = List.map (fun t -> t::path) successors in
        extend_path@paths)
        []
        paths
      in
        compute_cycles trans extended_paths cycles@res

  (** Computes all simple cycles which use t. *)
  let cycles_with_t trans t =
    compute_cycles trans [[t]] []
  
  (* determines all variables that get assigned a tmp value in a loop and removes them from the guard*)
  let update_path tmp_vars path = 
    let labels = List.map (List.first % Tuple3.second) path in
    let guard_vars = List.fold VarSet.union VarSet.empty @@ List.map (Guard.vars % TransitionLabel.guard) labels in
    let updates var = Util.cat_maybes @@ List.map (flip TransitionLabel.update var) labels in
    let non_statics = 
      let maybe_changed (non_statics, vars) = 
        let update_is_static u = VarSet.disjoint (UpdateElement.vars u) non_statics in
        let static_update non_statics var = List.for_all update_is_static @@ updates var in
        let new_non_statics, new_guard = VarSet.partition (not % static_update non_statics) vars in
        if VarSet.is_empty new_non_statics
          then MaybeChanged.same (VarSet.union new_non_statics non_statics, new_guard) 
          else MaybeChanged.changed (VarSet.union new_non_statics non_statics, new_guard) in
      Tuple2.first @@ Util.find_fixpoint maybe_changed (tmp_vars, guard_vars) in
    List.map (fun (x,l,y) -> (x,List.map (TransitionLabel.relax_guard ~non_static:non_statics) l,y)) path
    

  let logger = Logging.(get Twn)

  (* We contract a (shifted to start) cycle to a loop  *)
  let contract_cycle (cycle: path) start =
    let pre, post = List.span (fun (l,_,_) -> not @@ Location.equal start l) cycle in
    let merge (_,ts,_) = (List.map (Formula.mk % TransitionLabel.guard_without_inv) ts |> Formula.any, List.first ts |> TransitionLabel.update_map) in
    let merge_pre = List.map merge pre and merge_post = List.map merge post in
    List.fold Loop.append (List.first merge_post) (List.drop 1 merge_post@merge_pre)

  (** This method computes a loop for every entry transition of the cycle.
    Notice that we do not regard costs in the chaining step.
    However, we consider them when we compute the final bound. *)
  let chain_cycle ?(relevant_vars = None) cycle program =
    let entries = Program.entry_transitions logger program (handled_transitions cycle) in
    List.map (fun entry -> entry, contract_cycle cycle (Tuple3.third entry) |> Loop.eliminate_non_contributors ~relevant_vars) entries

  (** This function is used to obtain a set of loops which corresponds to simple cycles for corresponding entries. Used for TWN_Complexity. *)
  let find_loops ?(relevant_vars = None) ?(relax_loops=`NoRelaxation) ?(transformation_type = `NoTransformation) f appr program scc (l,t,l') =
    if relax_loops == `Relaxation || not @@ TransitionLabel.has_tmp_vars_in_guard t then
      let updated_trans = match relax_loops with
        | `Relaxation -> TransitionLabel.relax_guard t 
        | `NoRelaxation -> t in
        let handle_scc = match relax_loops with
        | `Relaxation -> List.map (fun (l,t,l') -> (l,TransitionLabel.relax_guard t,l')) 
        | `NoRelaxation -> List.filter (fun (l,t,l') -> not @@ TransitionLabel.has_tmp_vars_in_guard t) in 
        let merged_trans = Util.group (fun (l1,t,l1') (l2,t',l2') ->
          Location.equal l1 l2 &&
          Location.equal l1' l2' &&
          TransitionLabel.equivalent_update t t')
          (TransitionSet.to_list scc |> handle_scc)
          |> List.map (fun xs -> (Tuple3.first % List.first) xs, List.map Tuple3.second xs, (Tuple3.third % List.first) xs)
        in
        let merged_t = List.find_opt (fun (l1,ts,l1') ->
          List.exists (fun t1 -> TransitionLabel.equal updated_trans t1) ts
            && Location.equal l l1
            && Location.equal l' l1') merged_trans in
        let cycles = cycles_with_t merged_trans @@ Option.get merged_t in
        let tmp_vars = VarSet.diff (Program.vars program) (Program.input_vars program) in
        let handle_cycles = match relax_loops with
        | `Relaxation -> List.map @@ update_path tmp_vars 
        | `NoRelaxation -> identity in
        List.find_map_opt (fun cycle ->
          let chained_cycle = chain_cycle ~relevant_vars cycle program in
          if List.for_all (fun (entry,loop) -> f appr entry program loop) chained_cycle then
            Option.some
            (handled_transitions cycle,
            List.map (fun (entry,loop) -> (entry,Transformation.transform transformation_type loop)) chained_cycle)
          else
            None) @@ handle_cycles cycles
    else None 

  (** Computes update_n * ... * update_i where update_n is the update of a transition (_,_,target) and resp. update_i for (start,_,_). *)
  let traverse_cycle (cycle: path) start target =
    if Location.equal start target then
      TransitionLabel.update_map @@ List.first @@ Tuple3.second (List.first cycle)
    else
      let target_t = List.find (Location.equal target % Tuple3.third) cycle in
      let traversal = cycle@cycle
      |> List.drop_while (Location.equal start % Tuple3.first)
      |> List.take_while (not % Location.equal target % Tuple3.third)
      |> List.rev
      |> (@) [target_t] in
      let substitution update_map = fun var ->
        VarMap.Exceptionless.find var update_map |? Polynomial.of_var var
      in
      List.fold (fun map (_,ts,_) ->
        VarMap.map (Polynomial.substitute_f (substitution (TransitionLabel.update_map @@ List.first ts))) map)
        VarMap.empty traversal

  (** This function is used to obtain a loop which corresponds to a simple cycle. Used for SizeBounds. *)
  let find_loop ?(relevant_vars = None) f appr program scc (l,t,l') =
    if not @@ TransitionLabel.has_tmp_vars t then
      let merged_trans = Util.group (fun (l1,t,l1') (l2,t',l2') ->
        Location.equal l1 l2 &&
        Location.equal l1' l2' &&
        TransitionLabel.equivalent_update t t')
        (TransitionSet.to_list scc |> List.filter (not % TransitionLabel.has_tmp_vars % Tuple3.second))
        |> List.map (fun xs -> (Tuple3.first % List.first) xs, List.map Tuple3.second xs, (Tuple3.third % List.first) xs)
      in
      let merged_t = List.find (fun (l1,ts,l1') ->
        List.exists (fun t1 -> TransitionLabel.equal t t1) ts
          && Location.equal l l1
          && Location.equal l' l1') merged_trans in
      let cycles = cycles_with_t merged_trans merged_t in
      List.find_map_opt (fun cycle ->
        let loop = contract_cycle cycle l |> Loop.eliminate_non_contributors ~relevant_vars in
        if f appr program loop then
          let handled_transitions = handled_transitions cycle in
          (* Enlarge the cycle by transitions which do not have an influence on the size of the variables on the cycle. *)
          (* Entries of handled_transitions which are inside or outside of scc. *)
          let entries_inside,entries_outside =
            List.partition (flip TransitionSet.mem scc) (Program.entry_transitions logger program handled_transitions)
          in
          (* If a sub_scc changes a variable of the loop, then all entries in this sub_scc are relevant.
             Otherwise, we take the entries leading to this sub_scc which are not in the original scc. *)
          let sccs = TransitionGraph.sccs_ (List.enum @@ TransitionSet.(to_list @@ diff scc @@ of_list handled_transitions)) in
          let trans_in_scc = List.map TransitionSet.to_list sccs |> List.flatten |> TransitionSet.of_list in
          let relevant_entries = (
            List.map (fun sub_scc ->
            if TransitionSet.exists (fun (_,t,_) -> not @@ VarSet.disjoint (TransitionLabel.changed_vars t) (Loop.vars loop)) sub_scc then
              List.filter (flip TransitionSet.mem sub_scc) entries_inside
            else
              List.filter TransitionSet.(not % flip mem scc) @@ Program.entry_transitions logger program (TransitionSet.to_list sub_scc)
            ) sccs |> List.flatten)
            @
            (List.filter (not % flip TransitionSet.mem trans_in_scc) entries_inside) (* Transition which are not in an SCC. *)
          in
          Option.some (loop, List.map (fun entry -> entry, traverse_cycle cycle (Transition.src entry) l) (List.unique ~eq:Transition.equal relevant_entries@entries_outside))
        else
          None) cycles
    else
      None
end
