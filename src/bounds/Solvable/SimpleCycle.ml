open Batteries
open ProgramModules
open Formulas
open Atoms
open Polynomials
open ProgramTypes

module Loop = struct
  (** A loop is a 3-tuple (guard,invariant,update) *)
  type loop = Formula.t * Formula.t * (Polynomial.t VarMap.t)

  let to_string_loop (guard,inv,update) =
    let update_str =
      update
      |> VarMap.bindings
      |> List.map (fun (var,poly) -> (Var.to_string var, Polynomial.to_string poly))
      |> List.split
      |> fun (xs,ys) -> "("^(String.concat "," xs)^") -> ("^(String.concat "," ys)^")" in
    "(" ^ Formula.to_string guard ^ "," ^ Formula.to_string inv ^ "," ^ update_str

  (** Appends two loops. However, update invariants are moved to guards as appending does not maintain update invariance. *)
  let append ((guard,inv,update): loop) ((guard',inv',update'): loop) =
    let substitution update_map = fun var ->
      VarMap.Exceptionless.find var update_map |? Polynomial.of_var var
    in
    let new_update =
      VarMap.map (Polynomial.substitute_f (substitution update)) update'
    and new_guard =
      Formula.Infix.(guard && inv && Formula.map_polynomial (Polynomial.substitute_f (substitution update)) (guard' && inv'))
    in
      (new_guard,Formula.mk_true,new_update)

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

  (** A simple cycle l0 ->_{t_11 || ... || t_1m} ... ->_{t_n1 || ... || t_nk} ln with
    - p.w.d locations,
    - no temp vars, and
    - we alawys consider a list of transitions with the same update. *)
  type path = (Location.t * TransitionLabel.t list * Location.t) list

  let to_string t = ""

  (* Computes all simple cycles using transitions from {trans}. *)
  let rec compute_cycles trans (paths: path list) res =
    let (cycles, paths) =
      List.partition (fun path ->
        let
          l_end = List.first path |> Tuple3.first and
          l_start = List.last path |> Tuple3.third in
        Location.equal l_start l_end
      )
      paths
    in
    if List.is_empty paths then cycles@res
    else
      let extended_paths =
        List.fold (fun paths path ->
        let l_end = List.first path |> Tuple3.first in
        let successors = List.filter (
          fun (l,_,l') ->
            Location.equal l_end l (* successor transition *) &&
            not @@ List.exists (Location.equal l') (List.map Tuple3.third (path |> List.rev |> List.drop 1)) (* does not form a lasso *)
        ) trans in
        let extended_paths = List.map (fun t -> t::path) successors in
        extended_paths@paths)
        []
        paths
      in
        compute_cycles trans extended_paths cycles@res

  (** Computes all simple cycles which use t. *)
  let cycles_with_t trans t =
    compute_cycles trans [t] []

  (** This method computes a simple cycle which fulfills the properties of f and contains t *)
  let find_cycle f = None

  let logger = Logging.(get Twn)

  (* We contract a (shifted to start) cycle to a loop  *)
  let contract_cycle (cycle: path) start =
    let pre, post = List.span (fun (l,_,_) -> Location.equal start l) cycle in
    let merge (_,ts,_) = (List.map (Formula.mk % TransitionLabel.guard) ts |> Formula.any, Formula.mk_true, List.first ts |> TransitionLabel.update_map) in
    let merge_pre = List.map merge pre and merge_post = List.map merge post in
    List.fold Loop.append (List.first merge_post) ((List.tl merge_post)@merge_pre)

  (** This method computes a loop for every entry transition of the cycle.
    Notice that we do not regard costs in the chaining step.
    However, we consider them when we compute the final bound. *)
  let chain_cycle cycle program =
    let trans = List.fold (fun res (l,ts,l') -> List.map (fun t -> (l,t,l')) ts) [] cycle in
    let entries = Program.entry_transitions logger program trans in
    List.map (fun (l,t,l') -> (l,t,l'), contract_cycle cycle l') entries

  let find_cycle f appr program trans var t trans_ =
    let cycles = cycles_with_t trans_ t in
    List.find_map_opt (fun cycle ->
      let chained_cycle = chain_cycle cycle program in
      if List.for_all (fun (entry,loop) -> f appr program entry trans var loop) chained_cycle then
        Option.some chained_cycle
      else
        None) cycles

  (**
    - merge all transitionlabels
    - compute all cycles
    - take first which fulfills f (check)
    *)

end
