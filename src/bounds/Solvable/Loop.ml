open Batteries
open BoundsInst
open Formulas
open Polynomials

module Make(PM: ProgramTypes.ClassicalProgramModules) = struct
  open PM

  (** A loop is a 2-tuple (guard,update) *)
  type t = Formula.t * (Polynomial.t ProgramTypes.var_map)

  let mk t = (Formula.mk (TransitionLabel.guard t), TransitionLabel.update_map t)
  let id formula = (formula, Base.Map.empty (module Var))
  let guard = Tuple2.first
  let update = Tuple2.second
  let update_opt (_,update) var = Base.Map.find update var
  let update_var (_,update) var = Base.Map.find update var |? Polynomial.of_var var
  let updated_vars t = Base.Map.keys @@ update t |> VarSet.of_list
  let vars t =
    List.map (Polynomial.vars % update_var t) (Base.Set.to_list @@ updated_vars t)
    |> List.map Base.Set.to_list
    |> List.flatten
    |> VarSet.of_list
    |> Base.Set.union (updated_vars t)

  let to_string ((guard,update): t) =
    let update_str =
      update
      |> Base.Map.to_alist
      |> List.map (fun (var,poly) -> (Var.to_string ~pretty:true var, Polynomial.to_string_pretty poly))
      |> List.split
      |> fun (xs,ys) -> "("^(String.concat "," xs)^") -> ("^(String.concat "," ys)^")" in
    "(" ^ Formula.to_string ~pretty:true guard ^ "," ^ update_str

  (** Appends two loops. *)
  let append ((guard,update): t) ((guard',update'): t) =
    let substitution update_map = fun var ->
      Base.Map.find update_map var |? Polynomial.of_var var
    in
    let new_update =
      Base.Map.map ~f:(Polynomial.substitute_f (substitution update)) update'
    and new_guard =
      Formula.Infix.(guard  && Formula.map_polynomial (Polynomial.substitute_f (substitution update)) (guard'))
    in
      (new_guard,new_update)

  let chain t = append t t

  let eliminate_non_contributors ?(relevant_vars = None) (loop: t) =
    let f loop non_contributors =
      (guard loop, Base.Set.fold ~f:Base.Map.remove non_contributors ~init:(update loop)) in
    EliminateNonContributors.eliminate_t (updated_vars loop) (relevant_vars |? Formula.vars @@ guard loop) (update_opt loop) (f loop)

  let compute_bound_n_iterations (loop_org: t) var =
    let rec f (loop,bound) = function
    | 0 -> (loop,bound)
    | n -> let chained_loop = append loop loop_org in
      f (chained_loop, Bound.(add bound (of_poly @@ update_var chained_loop var))) (n - 1) in
    Tuple2.second % f (id Formula.mk_true,Bound.zero)
end
