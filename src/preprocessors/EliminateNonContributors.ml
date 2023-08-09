open OurBase
(** Implemenation of a preprocessor which eliminates variables that do not contribute to guards. *)

open Formulas
open Constraints
open Polynomials

module Make (M : ProgramTypes.ProgramModules) = struct
  open M

  let logger = Logging.(get Preprocessor)

  let depends var label =
    Set.exists ~f:(fun x ->
        TransitionLabel.update label x |? UpdateElement.zero |> UpdateElement.vars |> flip Set.mem var
        || TransitionLabel.cost label |> Polynomial.vars |> flip Set.mem var)


  let rec eliminate_t_ vars get_update contributors non_contributors =
    let xs, ys =
      Set.fold
        ~f:(fun (contr, non_contr) y ->
          if
            Set.exists ~f:(fun x -> Polynomial.vars (get_update x |? Polynomial.zero) |> flip Set.mem y) contr
          then
            (Set.add contr y, Set.remove non_contr y)
          else
            (contr, non_contr))
        vars ~init:(contributors, non_contributors)
    in
    if Set.equal non_contributors ys then
      contributors
    else
      eliminate_t_ vars get_update xs ys


  let rec eliminate_ program contributors non_contributors =
    let xs, ys =
      Base.Set.fold
        ~f:(fun (xs, ys) (l, t, l') ->
          Base.Set.fold
            ~f:(fun (contr, non_contr) y ->
              if depends y t contr then
                (Set.add contr y, Set.remove non_contr y)
              else
                (contr, non_contr))
            ys ~init:(xs, ys))
        (Program.transitions program) ~init:(contributors, non_contributors)
    in
    if Set.equal non_contributors ys then
      contributors
    else
      eliminate_ program xs ys


  let eliminate_t vars vars_guard get_update remove_non_contributors =
    let init_contr = vars_guard in
    let init_non_contr = Set.diff vars init_contr in
    Logger.(
      log logger INFO (fun () ->
          ( "EliminateNonContributors",
            [
              ("init_contr", VarSet.to_string init_contr);
              ("init_non_contributors", VarSet.to_string (Set.diff vars vars_guard));
            ] )));
    let contributors = eliminate_t_ vars get_update init_contr init_non_contr in
    let non_contributors = Set.diff vars contributors in
    remove_non_contributors non_contributors


  let eliminate program =
    let vars = Program.vars program in
    let vars_guard =
      Base.Set.fold
        ~f:(fun xs (l, t, l') -> Set.union (Constraint.vars (TransitionLabel.guard t)) xs)
        (Program.transitions program) ~init:VarSet.empty
    and vars_cost =
      Base.Set.fold
        ~f:(fun xs (l, t, l') -> Set.union (Polynomial.vars (TransitionLabel.cost t)) xs)
        (Program.transitions program) ~init:VarSet.empty
    in
    let init_contr = Set.union vars_guard vars_cost in
    Logger.(
      log logger INFO (fun () ->
          ( "EliminateNonContributors",
            [
              ("init_contr", VarSet.to_string init_contr);
              ("init_non_contributors", VarSet.to_string (Set.diff vars vars_guard));
            ] )));
    let contributors = eliminate_ program init_contr (Set.diff vars init_contr) in
    let non_contributors = Set.diff vars contributors in
    let program_ = Program.remove_non_contributors non_contributors program in
    Logger.(
      log logger INFO (fun () ->
          ("EliminateNonContributors", [ ("non_contributors", VarSet.to_string non_contributors) ])));
    if not (Set.is_empty non_contributors) then
      ProofOutput.add_str_paragraph_to_proof (fun () ->
          "Eliminate variables "
          ^ VarSet.to_string ~pretty:true non_contributors
          ^ " that do not contribute to the problem");
    if Set.is_empty non_contributors then
      MaybeChanged.same program
    else
      MaybeChanged.changed program_
end

include Make (ProgramModules)
