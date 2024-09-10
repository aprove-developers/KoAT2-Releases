open OurBase
open Formulas
open Polynomials

module Make (Bound : BoundType.Bound) (PM : ProgramTypes.ClassicalProgramModules) = struct
  open PM

  type t = Formula.t * Polynomial.t ProgramTypes.VarMap.t
  (** A loop is a 2-tuple (guard,update) *)

  let mk t = (Formula.mk (TransitionLabel.guard t), TransitionLabel.update_map t)
  let id formula = (formula, Map.empty (module Var))
  let guard = Tuple2.first
  let update = Tuple2.second
  let update_opt (_, update) var = Map.find update var
  let update_var (_, update) var = Map.find update var |? Polynomial.of_var var
  let updated_vars t = Map.keys @@ update t |> VarSet.of_list

  let vars t =
    List.map ~f:(Polynomial.vars % update_var t) (Set.to_list @@ updated_vars t)
    |> List.map ~f:Set.to_list |> List.concat |> VarSet.of_list
    |> Set.union (updated_vars t)


  let to_string ((guard, update) : t) =
    let update_str =
      update |> Map.to_alist
      |> List.map ~f:(fun (var, poly) -> (Var.to_string ~pretty:true var, Polynomial.to_string_pretty poly))
      |> List.unzip
      |> fun (xs, ys) -> "(" ^ String.concat ~sep:"," xs ^ ") -> (" ^ String.concat ~sep:"," ys ^ ")"
    in
    "("
    ^ (if Formula.is_true guard then
         "true"
       else
         Formula.to_string ~pretty:true guard)
    ^ "," ^ update_str ^ ")"


  (** Appends two loops. *)
  let append ((guard, update) : t) ((guard', update') : t) =
    let substitution update_map var = Map.find update_map var |? Polynomial.of_var var in
    let new_update = Map.map ~f:(Polynomial.substitute_f (substitution update)) update'
    and new_guard =
      Formula.Infix.(guard && Formula.map_polynomial (Polynomial.substitute_f (substitution update)) guard')
    in
    (new_guard, new_update)


  let chain t = append t t

  let eliminate_non_contributors ?(relevant_vars = None) (loop : t) =
    let f loop non_contributors = (guard loop, Set.fold ~f:Map.remove non_contributors ~init:(update loop)) in
    EliminateNonContributors.eliminate_t (updated_vars loop)
      (relevant_vars |? Formula.vars @@ guard loop)
      (update_opt loop) (f loop)


  let compute_bound_n_iterations (loop_org : t) var =
    let rec f (loop, bound) = function
      | 0 -> (loop, bound)
      | n ->
          let chained_loop = append loop loop_org in
          f (chained_loop, Bound.(add bound (of_intpoly @@ update_var chained_loop var))) (n - 1)
    in
    Tuple2.second % f (id Formula.mk_true, Bound.zero)


  let substition_unsolvable loop q x =
    let substition q p =
      let opt = Polynomial.find_common_factor p q in
      if Option.is_none opt then
        p
      else
        let common_factor = Option.value_exn opt in
        Polynomial.(
          mult_with_const (OurRational.den common_factor) p
          - mult_with_const (OurRational.num common_factor) q
          + (mult_with_const (OurRational.num common_factor) @@ of_var x))
    in
    let new_guard = Formula.map_polynomial (substition q) (guard loop) in
    let new_update =
      Map.add_exn (update loop) ~key:x ~data:(Polynomial.substitute_f (update_var loop) q |> substition q)
    in
    (new_guard, new_update)


  let commuting loop1 loop2 =
    Map.equal Polynomial.equal (update @@ append loop1 loop2) (update @@ append loop2 loop1)


  let check_update_invariant (loop : t) atom =
    let open Atoms in
    let module SMTSolver = SMT.Z3Solver in
    let poly = Atom.poly_lt atom in
    let poly_updated = Polynomial.substitute_f (update_var loop) poly in
    let atom_updated = Atom.mk_lt poly_updated Polynomial.zero in
    SMTSolver.tautology Formula.(implies (mk [ atom ]) (mk [ atom_updated ]))
end
