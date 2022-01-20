open Batteries
open Polynomials
open ProgramTypes
open Atoms

module Guard = Formulas.Formula
module Invariant = Constraints.Constraint
type polynomial = Polynomial.t
module VarMap = Map.Make(Var)

type t = {
  id: int;
  update : Polynomial.t VarMap.t;
  guard : Guard.t;
  invariant : Invariant.t;
  subsumed_transitionlabels : TransitionLabel.t list; (* Transitions that are subsumed by this t *)
}

let update t var = VarMap.Exceptionless.find var t.update

let update_map t = t.update

let guard t = Guard.mk_and t.guard (Guard.lift [t.invariant])

let guard_without_inv t = t.guard

let invariant t = t.invariant

let subsumed_transitionlabels t = t.subsumed_transitionlabels
let subsumed_transitions l l' twn = List.map (fun t -> l,t,l') twn.subsumed_transitionlabels

let mk_transitions xs =
let invariants = List.map TransitionLabel.invariant xs in
let common_invariants = List.flatten invariants |> List.filter (fun atom -> List.for_all (List.exists (Atom.equal atom)) invariants) in
{
    id = unique();
    update = List.first xs |> TransitionLabel.update_map;
    guard = Guard.lift (List.map TransitionLabel.guard_without_inv xs);
    invariant = common_invariants;
    subsumed_transitionlabels = xs;
}

let remove_non_contributors t non_contributors =
  { t with update = VarSet.fold VarMap.remove non_contributors t.update }

let mk_transition t = mk_transitions [t]

let add_invariant t inv =
  {t with invariant = Invariant.mk_and t.invariant inv}

let append t1 t2 =
  let module VarTable = Hashtbl.Make(Var) in
  let nondet_vars = VarTable.create 3 in
  let substitution update_map var =
    VarMap.Exceptionless.find var update_map
    |? Polynomial.of_var
         (* Variables which are nondeterministic in the preceding transition are represented by fresh variables. *)
         (VarTable.find_option nondet_vars var
          |> Option.default_delayed (fun () ->
                 let nondet_var = Var.fresh_id Var.Int () in
                 VarTable.add nondet_vars var nondet_var;
                 nondet_var
               )
         )
  in
  let new_update =
    VarMap.map (Polynomial.substitute_f (substitution t1.update)) t2.update
  and new_guard =
    Guard.Infix.(t1.guard && Guard.map_polynomial (Polynomial.substitute_f (substitution t1.update)) t2.guard)
  and new_invariant =
    Invariant.Infix.(t1.invariant && Invariant.map_polynomial (Polynomial.substitute_f (substitution t1.update)) t2.invariant)
  in
  { id = unique ();
    update = new_update;
    guard = new_guard;
    invariant = new_invariant;
    subsumed_transitionlabels = t1.subsumed_transitionlabels@t2.subsumed_transitionlabels;
  }

let chain t =
  let module VarTable = Hashtbl.Make(Var) in
  let nondet_vars = VarTable.create 3 in
  let substitution update_map var =
    VarMap.Exceptionless.find var update_map
    |? Polynomial.of_var
         (* Variables which are nondeterministic in the preceding transition are represented by fresh variables. *)
         (VarTable.find_option nondet_vars var
          |> Option.default_delayed (fun () ->
                 let nondet_var = Var.fresh_id Var.Int () in
                 VarTable.add nondet_vars var nondet_var;
                 nondet_var
               )
         )
  in
  let new_update =
    VarMap.map (Polynomial.substitute_f (substitution t.update)) t.update in
  {t with update = new_update}


let update_to_string update =
  if VarMap.is_empty update then
    "true"
  else
    update
    |> VarMap.bindings
    |> List.map (fun (var,poly) -> (Var.to_string var, Polynomial.to_string poly))
    |> List.split
    |> fun (xs,ys) -> "("^(String.concat "," xs)^") -> ("^(String.concat "," ys)^")"


let guard_to_string ?(pretty = false) label =
  if
    Guard.is_true (Guard.mk_and label.guard (Guard.mk label.invariant)) then ""
  else
    Guard.to_string ~pretty (Guard.mk_and label.guard (Guard.mk label.invariant))

let guard_without_inv_to_string  ?(pretty = false) label =
  if
    Guard.is_true label.guard then ""
  else
    Guard.to_string  ~pretty label.guard

let invariant_to_string  ?(pretty = false) label =
  if
    Invariant.is_true label.invariant then ""
  else
    Invariant.to_string  ~pretty label.invariant

let update_to_string_lhs  t =
  let update = t.update in
    if VarMap.is_empty update then
      ""
    else
      update
      |> VarMap.bindings
      |> List.map (fun (var,poly) -> (Var.to_string  var, Polynomial.to_string poly))
      |> List.split
      |> Tuple2.first
      |> fun xs -> "("^(String.concat "," xs)^")"

let update_to_string_rhs  t =
  let update = t.update in
    if VarMap.is_empty update then
        ""
    else
      update
      |> VarMap.bindings
      |> List.map (fun (var,poly) -> (Var.to_string  var, Polynomial.to_string poly))
      |> List.split
      |> Tuple2.second
      |> fun xs -> "("^(String.concat "," xs)^")"

let update_to_string_lhs_pretty t =
  let update = t.update in
    if VarMap.is_empty update then
      ""
    else
      update
      |> VarMap.bindings
      |> List.map (fun (var,poly) -> (Var.to_string ~pretty:true var, Polynomial.to_string_pretty poly))
      |> List.split
      |> Tuple2.first
      |> fun xs -> "("^(String.concat "," xs)^")"

let update_to_string_rhs_pretty t =
  let update = t.update in
    if VarMap.is_empty update then
      ""
    else
      update
      |> VarMap.bindings
      |> List.map (fun (var,poly) -> (Var.to_string ~pretty:true var, (Polynomial.to_string_pretty poly)))
      |> List.split
      |> Tuple2.second
      |> fun xs -> "("^(String.concat "," xs)^")"

let to_string ?(pretty = false) label =
  let guard = if Guard.is_true label.guard  then "" else " :|: " ^ guard_without_inv_to_string ~pretty label in
  let invariant = if Invariant.is_true label.invariant  then "" else "Inv: [" ^ invariant_to_string ~pretty label ^ "] , "in
  if pretty then
    "twn" ^ ":" ^ invariant ^ " " ^ update_to_string_lhs_pretty label ^ " -> " ^  update_to_string_rhs_pretty label ^ guard
  else
    invariant  ^ update_to_string label.update ^ guard

let vars {update; guard; invariant; _} =
  VarMap.fold (fun _ -> VarSet.union % Polynomial.vars) update VarSet.empty
  |> (VarSet.union % VarSet.of_enum % VarMap.keys) update
  |> (VarSet.union % Guard.vars) guard
  |> (VarSet.union % Invariant.vars) invariant

let input_vars t =
  VarMap.keys t.update
  |> VarSet.of_enum

