open! OurBase
(** Implemenation of a preprocessor which eliminates variables that do not contribute to guards. *)

open Constraints
open Polynomials

module type Adapter = sig
  type transition
  type transition_label

  val vars_with_function_call_calls : transition -> VarSet.t
  val dependency_rec : transition_label -> Var.t -> Var.t -> bool
end

module ClassicAdapter (M : ProgramTypes.ClassicalProgramModules) = struct
  open M

  type transition = Transition.t
  type transition_label = TransitionLabel.t

  (* Returns the variables which have a recursive call. We cannot remove them. *)
  let vars_with_function_call_calls (_, t, _) =
    TransitionLabel.input_vars t
    |> Set.filter ~f:(fun x ->
           TransitionLabel.update t x |? PolyFunctionCall.of_var x |> PolyFunctionCall.has_function_calls)


  (* Returns true iff x depends on y in a recursive call of t. *)
  let dependency_rec t x y =
    let vars_with_function_call = TransitionLabel.function_call_vars t in
    Set.exists vars_with_function_call ~f:(fun var_function_call ->
        Set.mem (VarFunctionCall.dependencies (TransitionLabel.input_vars t) x var_function_call) y
        || Var.equal y (VarFunctionCall.return_var var_function_call))
end

module DefaultAdapter = struct
  type transition
  type transition_label

  let vars_with_function_call_calls t = VarSet.empty
  let dependency_rec t x y = false
end

module Make
    (M : ProgramTypes.ProgramModules)
    (A : Adapter with type transition := M.Transition.t and type transition_label := M.TransitionLabel.t) =
struct
  open M

  let depends var label =
    Set.exists ~f:(fun x ->
        TransitionLabel.update label x |? UpdateElement.zero |> UpdateElement.vars |> flip Set.mem var
        || TransitionLabel.cost label |> Polynomial.vars |> flip Set.mem var
        || A.dependency_rec label x var)


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


  let rec compute_contributors_ transitionset contributors non_contributors =
    let xs, ys =
      Set.fold
        ~f:(fun (xs, ys) (l, t, l') ->
          Set.fold
            ~f:(fun (contr, non_contr) y ->
              if depends y t contr then
                (Set.add contr y, Set.remove non_contr y)
              else
                (contr, non_contr))
            ys ~init:(xs, ys))
        transitionset ~init:(contributors, non_contributors)
    in
    if Set.equal non_contributors ys then
      contributors
    else
      compute_contributors_ transitionset xs ys


  let compute_contributors transitionset =
    let all_vars =
      Set.to_list transitionset |> List.map ~f:(TransitionLabel.vars % Transition.label) |> VarSet.union_list
    in
    let vars_guard =
      Set.to_list transitionset
      |> List.map ~f:(Constraint.vars % TransitionLabel.guard % Transition.label)
      |> VarSet.union_list
    in
    let vars_with_function_call = List.map ~f:A.vars_with_function_call_calls (Set.to_list transitionset) |> VarSet.union_list in
    let vars = Set.union vars_with_function_call vars_guard in
    compute_contributors_ transitionset vars (Set.diff all_vars vars)


  let eliminate_ program = compute_contributors_ (Program.transitions program)

  let eliminate_t logger vars vars_guard get_update remove_non_contributors =
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


  let eliminate logger program =
    let vars = Program.vars program in
    let vars_guard =
      Set.fold
        ~f:(fun xs (l, t, l') -> Set.union (Constraint.vars (TransitionLabel.guard t)) xs)
        (Program.transitions program) ~init:VarSet.empty
    and vars_cost =
      Set.fold
        ~f:(fun xs (l, t, l') -> Set.union (Polynomial.vars (TransitionLabel.cost t)) xs)
        (Program.transitions program) ~init:VarSet.empty
    in
    let vars_with_function_call =
      List.map ~f:A.vars_with_function_call_calls (Set.to_list @@ Program.transitions program) |> VarSet.union_list
    in
    if Set.is_empty vars_with_function_call then (
      let init_contr = VarSet.union_list [ vars_with_function_call; vars_guard; vars_cost ] in
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
        MaybeChanged.changed program_)
    else
      MaybeChanged.same program
end

include Make (ProgramModules) (ClassicAdapter (ProgramModules))
