open Batteries
open Formulas
open Polynomials

module VarMap = Map.Make(Var)

module Make(PM: ProgramTypes.ClassicalProgramModules) = struct
  open PM

  (** A loop is a 2-tuple (guard,update) *)
  type t = Formula.t * (Polynomial.t VarMap.t)

  let mk t = (Formula.mk (TransitionLabel.guard t), TransitionLabel.update_map t)
  let guard = Tuple2.first
  let update = Tuple2.second
  let update_opt (_,update) var = VarMap.find_opt var update
  let update_var (_,update) var = VarMap.find_opt var update |? Polynomial.of_var var
  let updated_vars t = VarMap.keys @@ update t |> VarSet.of_enum

  let to_string ((guard,update): t) =
    let update_str =
      update
      |> VarMap.bindings
      |> List.map (fun (var,poly) -> (Var.to_string ~pretty:true var, Polynomial.to_string_pretty poly))
      |> List.split
      |> fun (xs,ys) -> "("^(String.concat "," xs)^") -> ("^(String.concat "," ys)^")" in
    "(" ^ Formula.to_string ~pretty:true guard ^ "," ^ update_str

  (** Appends two loops. *)
  let append ((guard,update): t) ((guard',update'): t) =
    let substitution update_map = fun var ->
      VarMap.Exceptionless.find var update_map |? Polynomial.of_var var
    in
    let new_update =
      VarMap.map (Polynomial.substitute_f (substitution update)) update'
    and new_guard =
      Formula.Infix.(guard  && Formula.map_polynomial (Polynomial.substitute_f (substitution update)) (guard'))
    in
      (new_guard,new_update)

  let chain t = append t t

  let eliminate_non_contributors ?(relevant_vars = None) (loop: t) =
    let f loop non_contributors =
      (guard loop, VarSet.fold VarMap.remove non_contributors (update loop)) in
    EliminateNonContributors.eliminate_t (updated_vars loop) (relevant_vars |? Formula.vars @@ guard loop) (update_opt loop) (f loop)
end
