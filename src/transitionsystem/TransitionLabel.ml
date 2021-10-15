open Batteries
open Polynomials
open Formulas

module Guard = Constraints.Constraint
module Invariant = Constraints.Constraint
type polynomial = Polynomial.t
module VarMap = Map.Make(Var)

type kind = [ `Lower | `Upper ] [@@deriving eq, ord]

type t = {
    id : int;
    update : Polynomial.t VarMap.t;
    guard : Guard.t;
    invariant :Invariant.t;
    cost : Polynomial.t;
  }

let one = Polynomial.one

let make ~cost ~update ~guard ~invariant =
  {
    id = unique ();
    update; guard; invariant; cost;
  }

let fresh_id t = {
    id = unique ();
    update = t.update;
    guard = t.guard;
    invariant = t.invariant;
    cost = t.cost;
  }

let trival variables =
  let var_map =
    VarSet.fold (fun var map -> VarMap.add var (Polynomial.of_var var) map) variables VarMap.empty in
  make ~cost:Polynomial.one ~update:var_map ~guard:Guard.mk_true ~invariant:Invariant.mk_true

let same lbl1 lbl2 =
  lbl1.id = lbl2.id

let equivalent lbl1 lbl2 =
  VarMap.equal Polynomial.equal lbl1.update lbl2.update
  && Guard.equal lbl1.guard lbl2.guard
  && Invariant.equal lbl1.invariant lbl2.invariant
  && Polynomial.equal lbl1.cost lbl2.cost

let compare_same lbl1 lbl2 =
  Int.compare lbl1.id lbl2.id

let compare_equivalent lbl1 lbl2 =
  if VarMap.compare Polynomial.compare lbl1.update lbl2.update != 0 then
    VarMap.compare Polynomial.compare lbl1.update lbl2.update
  else if Guard.compare lbl1.guard lbl2.guard != 0 then
    Guard.compare lbl1.guard lbl2.guard
  else if Invariant.compare lbl1.invariant lbl2.invariant != 0 then
    Invariant.compare lbl1.invariant lbl2.invariant
  else if Polynomial.compare lbl1.cost lbl2.cost != 0 then
    Polynomial.compare lbl1.cost lbl2.cost
  else
    0
let take_last n xs =
  xs
  |> List.rev
  |> List.take n
  |> List.rev

(* TODO Pattern <-> Assigment relation *)
let mk ~cost ~assignments ~patterns ~guard ~vars =
  let assignments_with_trivial =
      assignments @ List.map Polynomial.of_var (take_last ((List.length patterns) - (List.length assignments)) patterns) in
  let appended_patterns =
      patterns @ Var.fresh_id_list Var.Int ((List.length assignments) - (List.length patterns)) in
  (* TODO Better error handling in case the sizes differ *)
  (List.enum appended_patterns, List.enum assignments_with_trivial)
  |> (uncurry Enum.combine)
  |> Enum.map (fun (var, assignment) -> VarMap.add var assignment)
  |> Enum.fold (fun map adder -> adder map) VarMap.empty
  |> fun update -> { id = unique ();
                     update; guard; 
                     invariant = Invariant.mk_true; 
                     cost;}

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
  and updated_cost = Polynomial.substitute_f (substitution t1.update) t2.cost
  in
  {
    id = unique ();
    update = new_update;
    guard = new_guard;
    invariant = new_invariant;
    cost = Polynomial.(t1.cost + updated_cost);
  }

let id t = t.id

let update t var = VarMap.Exceptionless.find var t.update

let update_map t = t.update

module Monomial = Monomials.Make(OurInt)
let overapprox_nonlinear_updates t =
  let overapprox_poly var poly (guard, update) =
    if Polynomial.is_linear poly then guard, update
    else
      let new_var = Var.fresh_id Var.Int () in
      let new_var_poly = Polynomial.of_var new_var in
      match Polynomial.monomials_with_coeffs poly with
        | [(coeff,mon)] ->
            (** check if update is quadratic, ^4, ^6, ... *)
            let vars = Monomial.vars mon in
            if VarSet.cardinal vars = 1 then
              if (Monomial.degree_variable var mon) mod 2 = 0 && Var.equal (VarSet.any vars) var then
                let var_poly = Polynomial.of_var var in
                (* check if variable can be zero. drop nonlinear to ensure fast termination of SMT call *)
                let formula = Formula.mk_and
                              (Formula.mk_and  (Formula.mk_lt var_poly (Polynomial.of_int 2))
                                               (Formula.mk_gt var_poly (Polynomial.of_int (-2))))
                              (Formula.mk @@ Guard.drop_nonlinear t.guard)
                in
                if SMT.Z3Solver.unsatisfiable formula then
                  (* The update will increase the variable (disregarding factor) *)
                  Guard.mk_and (Guard.mk_gt new_var_poly var_poly) guard, VarMap.add var (Polynomial.mul (Polynomial.of_constant coeff) new_var_poly) update
                else
                  (* updated variable will be non-negative (disregarding factor) *)
                  Guard.mk_and (Guard.mk_ge new_var_poly Polynomial.zero) guard, VarMap.add var (Polynomial.mul (Polynomial.of_constant coeff) new_var_poly) update
              else
                guard, VarMap.add var new_var_poly update
            else
              guard, VarMap.add var new_var_poly update

        | _ -> Guard.mk_true, VarMap.add var new_var_poly update
  in
  let (guard',update') = VarMap.fold overapprox_poly t.update (t.guard, t.update) in
  {t with guard = guard'; update = update'}

let guard t = Guard.mk_and t.guard t.invariant

let guard_without_inv t = t.guard

let invariant t = t.invariant

let add_invariant t invariant' = {t with invariant = (Invariant.mk_and (t.invariant) invariant');}

let map_guard f label =
  { label with guard = f label.guard; invariant = f label.invariant}

let cost t = t.cost

let vars_ {update; guard; invariant; cost; _} =
  VarMap.fold (fun _ -> VarSet.union % Polynomial.vars) update VarSet.empty
  |> (VarSet.union % VarSet.of_enum % VarMap.keys) update
  |> (VarSet.union % Guard.vars) guard
  |> (VarSet.union % Invariant.vars) invariant
  |> (VarSet.union % Polynomial.vars) cost

(* TODO May invalidate through invariant generation! *)
let vars_memoization: (int,VarSet.t) Hashtbl.t = Hashtbl.create 10
let vars = Util.memoize vars_memoization ~extractor:id vars_

let vars_update t = (VarSet.of_enum % VarMap.keys) t.update

let default = {
    id = 0;
    update = VarMap.empty;
    guard = Guard.mk_true;
    invariant = Invariant.mk_true;
    cost = one;
  }

let update_to_string update =
  if VarMap.is_empty update then
    "true"
  else
    update
    |> VarMap.bindings
    |> List.map (fun (var,poly) -> (Var.to_string var, Polynomial.to_string poly))
    |> List.split
    |> fun (xs,ys) -> "("^(String.concat "," xs)^") -> ("^(String.concat "," ys)^")"

let cost_to_string ?(to_file = false) label =
  if
    Polynomial.is_one label.cost then ""
  else
    "{"^(Polynomial.to_string label.cost)^"}"


let guard_to_string ?(to_file = false) ?(pretty = false) label =
  if
    Guard.is_true (Guard.mk_and label.guard label.invariant) then ""
  else
    Guard.to_string ~to_file ~pretty ~conj:(if pretty then " ∧ " else " && ") (Guard.mk_and label.guard label.invariant)

let normalise t (input_vars:VarSet.t) = {
    id = t.id;
    update = VarSet.fold (fun var fold_update -> if (VarMap.mem var fold_update)
                                            then fold_update
                                            else VarMap.add var (Polynomial.of_var var) fold_update) input_vars t.update;
    guard = t.guard;
    invariant = t.invariant;
    cost = t.cost;
  }

let update_to_string_lhs ?(to_file = false) t =
  let update = t.update in
    if VarMap.is_empty update then
      if to_file then "()" else ""
    else
      update
      |> VarMap.bindings
      |> List.map (fun (var,poly) -> (Var.to_string ~to_file var, (if to_file then (Polynomial.to_string_to_file poly) else (Polynomial.to_string poly))))
      |> List.split
      |> Tuple2.first
      |> fun xs -> "("^(String.concat "," xs)^")"

let update_to_string_rhs ?(to_file = false) t =
  let update = t.update in
    if VarMap.is_empty update then
      if to_file then "()" else ""
    else
      update
      |> VarMap.bindings
      |> List.map (fun (var,poly) -> (Var.to_string ~to_file var, (if to_file then (Polynomial.to_string_to_file poly) else (Polynomial.to_string poly))))
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
      |> List.map (fun (var,poly) -> (Var.to_string_index var, (Polynomial.to_string_pretty poly)))
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
      |> List.map (fun (var,poly) -> (Var.to_string_index var, (Polynomial.to_string_pretty poly)))
      |> List.split
      |> Tuple2.second
      |> fun xs -> "("^(String.concat "," xs)^")"

let to_string ?(pretty = false) label =
  let guard = if Guard.is_true (Guard.mk_and label.guard label.invariant) then "" else " :|: " ^ guard_to_string ~pretty label in
  let cost = if Polynomial.is_one label.cost then "" else if pretty then Polynomial.to_string_pretty label.cost else Polynomial.to_string label.cost in
  if pretty then
    "t" ^ (Util.natural_to_index label.id) ^ ": " ^ update_to_string_lhs_pretty label ^ " -{" ^ cost ^ "}> "  ^ update_to_string_rhs_pretty label ^ guard
  else 
    "ID: " ^ string_of_int label.id ^ ", " ^ cost ^ "&euro;" ^ ", " ^ update_to_string label.update ^ guard

let to_id_string =
  string_of_int % id

let input_vars t =
  t.update
  |> VarMap.keys
  |> VarSet.of_enum

let input_size t =
  t
  |> input_vars
  |> VarSet.cardinal

(** Whenever this function is invoked it is ensured that there are enough standard variables  *)
let standard_renaming standard_vars t =
  standard_vars
  |> List.take (input_size t)
  |> List.combine ((VarSet.elements % input_vars) t)
  |> RenameMap.from

let rename_update update rename_map =
  update
  |> VarMap.enum
  |> Enum.map (fun (key, value) -> (RenameMap.find key rename_map ~default:key), Polynomial.rename rename_map value)
  |> VarMap.of_enum


let rename_temp_vars t temp_vars =
  let temp_vars_of_label = VarSet.diff (vars t) (input_vars t) in
  let rename_map =
    List.mapi (fun i v -> v, LazyList.at temp_vars i) @@ VarSet.to_list temp_vars_of_label
    |> RenameMap.from
  in
  {
    id = t.id;
    update = rename_update t.update rename_map;
    guard = Guard.rename t.guard rename_map;
    invariant = Invariant.rename t.invariant rename_map;
    cost = Polynomial.rename rename_map t.cost;
  }
  |> tap (fun _ -> Hashtbl.clear vars_memoization)


let rename standard_vars t =
  let rename_map = standard_renaming standard_vars t in
  {
    id = t.id;
    update = rename_update t.update rename_map;
    guard = Guard.rename t.guard rename_map;
    invariant = Invariant.rename t.invariant rename_map;
    cost = Polynomial.rename rename_map t.cost;
  }


let rename2 rename_map t =
  {
    id = t.id;
    update = rename_update t.update rename_map;
    guard = Guard.rename t.guard rename_map;
    invariant = Invariant.rename t.invariant rename_map;
    cost = Polynomial.rename rename_map t.cost;
  }

let remove_non_contributors non_contributors t =
    let update_ = VarSet.fold (fun var u -> VarMap.remove var u) non_contributors t.update in
    {
    id = unique ();
    update = update_;
    guard = t.guard;
    invariant = t.invariant;
    cost = t.cost;
    }

(* We execute CFRefinement with guard && invariant -> We need to separate invariant afterwards. *)
let separate_guard_invariant t invariant' = {t with guard = List.filter (fun atom -> List.exists (fun atom_inv -> Atoms.Atom.equal atom atom_inv) invariant' |> not) t.guard;
                                                    invariant = List.filter (fun atom -> List.exists (fun atom_inv -> Atoms.Atom.equal atom atom_inv) invariant') t.guard}