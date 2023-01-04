open Batteries
open ProgramModules
open Formulas
open Constraints
open Atoms
open Polynomials
open ProgramTypes

module Loop(PM: ProgramTypes.ClassicalProgramModules) = struct
  open PM

  (** A loop is a 3-tuple (guard,invariant,update) *)
  type t = Formula.t * Constraint.t * (Polynomial.t VarMap.t)

  let mk t = (Formula.mk (TransitionLabel.guard t), TransitionLabel.invariant t, TransitionLabel.update_map t)
  let guard = Tuple3.first
  let inv = Tuple3.second
  let update = Tuple3.third
  let update_var (_,_,update) var = VarMap.find_opt var update |? Polynomial.of_var var
  let updated_vars t = VarMap.keys (update t) |> VarSet.of_enum

  let to_string ((guard,inv,update): t) =
    let update_str =
      update
      |> VarMap.bindings
      |> List.map (fun (var,poly) -> (Var.to_string ~pretty:true var, Polynomial.to_string_pretty poly))
      |> List.split
      |> fun (xs,ys) -> "("^(String.concat "," xs)^") -> ("^(String.concat "," ys)^")" in
    "(" ^ Formula.to_string ~pretty:true guard ^ "," ^ Constraint.to_string ~pretty:true inv ^ "," ^ update_str

  (** Appends two loops. However, update invariants are removed as appending does not maintain update invariance. *)
  let append ((guard,_,update): t) ((guard',_,update'): t) =
    let substitution update_map = fun var ->
      VarMap.Exceptionless.find var update_map |? Polynomial.of_var var
    in
    let new_update =
      VarMap.map (Polynomial.substitute_f (substitution update)) update'
    and new_guard =
      Formula.Infix.(guard  && Formula.map_polynomial (Polynomial.substitute_f (substitution update)) (guard'))
    in
      (new_guard,Constraint.mk_true,new_update)

  module SMTSolver = SMT.Z3Solver

  (** Checks if an atom a is an update invariant, i.e., |= a -> update(a). *)
  let check_update_invariant (guard,_,update) atom =
    let poly = Atom.poly atom in
    let substitution update_map = fun var ->
      VarMap.Exceptionless.find var update_map |? Polynomial.of_var var
    in
    let poly_updated = Polynomial.substitute_f (substitution update) poly in
    let atom_updated = Atom.mk_le poly_updated Polynomial.zero in
    SMTSolver.tautology Formula.(implies (mk [atom]) (mk [atom_updated]))
end

module SimpleCycle(PM: ProgramTypes.ClassicalProgramModules) = struct
  open PM

  module Loop = Loop(PM)

  (** A simple cycle l0 ->_{t_11 || ... || t_1m} ... ->_{t_n1 || ... || t_nk} ln with
    - p.w.d locations,
    - no temp vars, and
    - we always consider a list of transitions with the same update. *)
  type path = (Location.t * TransitionLabel.t list * Location.t) list

  let to_string path = List.map (fun (l,ts,l') -> Location.to_string l ^ List.fold_right (fun t str -> str ^ TransitionLabel.to_string t) ts "" ^ Location.to_string l') path |> String.concat ""

  let handled_transitions t =
    List.flatten (List.map (fun (l,ts,l') -> List.fold_right (fun t res -> (l,t,l')::res) ts []) t)

  (* Computes all simple cycles using transitions from {trans}. *)
  let rec compute_cycles trans (paths: path list) res =
    (* Separate already completed cycles from "open" paths. *)
    let (cycles, paths) =
      List.partition (fun path ->
        let
          l_start = List.first path |> Tuple3.first and
          l_end = List.last path |> Tuple3.third in
        Location.equal l_start l_end
      )
      paths
    in
    if List.is_empty paths then cycles@res
    (* Extend "open" paths. *)
    else
      let extended_paths =
        List.fold (fun paths path ->
        let l_end = List.last path |> Tuple3.third in
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

  let logger = Logging.(get Twn)

  (* We contract a (shifted to start) cycle to a loop  *)
  let contract_cycle (cycle: path) start =
    let pre, post = List.span (fun (l,_,_) -> not @@ Location.equal start l) cycle in
    let merge (_,ts,_) = (List.map (Formula.mk % TransitionLabel.guard) ts |> Formula.any, Constraint.mk_true, List.first ts |> TransitionLabel.update_map) in
    let merge_pre = List.map merge pre and merge_post = List.map merge post in
    List.fold Loop.append (List.first merge_post) (List.drop 1 merge_post@merge_pre)

  (** This method computes a loop for every entry transition of the cycle.
    Notice that we do not regard costs in the chaining step.
    However, we consider them when we compute the final bound. *)
  let chain_cycle cycle program =
    let entries = Program.entry_transitions logger program (handled_transitions cycle) in
    List.map (fun entry -> entry, contract_cycle cycle (Tuple3.third entry)) entries

  let find_loops appr program scc t = (* TODO add var *)
    let merged_trans = Util.group (fun (l1,t,l1') (l2,t',l2') ->
      Location.equal l1 l2 &&
      Location.equal l1' l2' &&
      TransitionLabel.equivalent t t') (TransitionSet.to_list scc)
      |> List.map (fun xs -> (Tuple3.first % List.first) xs, List.map Tuple3.second xs, (Tuple3.third % List.first) xs)
    in
    let merged_t = List.find (List.exists (fun t' -> TransitionLabel.equivalent t t') % Tuple3.second) merged_trans in
    let cycles = cycles_with_t merged_trans merged_t in
    List.find_map_opt (fun cycle ->
      let chained_cycle = chain_cycle cycle program in
      if List.for_all (fun (entry,loop) -> true (* TODO f appr program entry trans var loop *)) chained_cycle then
        Option.some chained_cycle
      else
        None) cycles

  (**
    - merge all transitionlabels
    - compute all cycles
    - take first which fulfills f (check)
    *)

end
