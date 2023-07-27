open Batteries
open Polynomials
open Formulas
open Atoms

module Invariant = Guard
module Monomial = Monomials.Make(OurInt)
module VarMap = ProgramTypes.VarMap

type update_element = Polynomial.t

type t = {
    id : int;
    update : Polynomial.t VarMap.t;
    guard : Guard.t;
    invariant : Invariant.t;
    cost : Polynomial.t;
  }

let fresh_id t = {
    id = unique ();
    update = t.update;
    guard = t.guard;
    invariant = t.invariant;
    cost = t.cost;
  }

let equivalent lbl1 lbl2 =
  VarMap.equal Polynomial.equal lbl1.update lbl2.update
  && Guard.equal lbl1.guard lbl2.guard
  && Invariant.equal lbl1.invariant lbl2.invariant
  && Polynomial.equal lbl1.cost lbl2.cost

let equal lbl1 lbl2 =
  Int.equal lbl1.id lbl2.id
let compare lbl1 lbl2 =
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

let equivalent_update lbl1 lbl2 =
  VarMap.equal Polynomial.equal lbl1.update lbl2.update

let fill_up_update_arg_vars_up_to_num n update =
  let missing_args =
    VarSet.diff
      (VarSet.of_enum % Enum.take n @@ LazyList.enum Var.args)
      (VarSet.of_enum @@ VarMap.keys update)
  in
  VarSet.fold (fun v -> VarMap.add v (Polynomial.of_var v)) missing_args update

let fill_up_arg_vars_up_to_num n t =
  {t with update = fill_up_update_arg_vars_up_to_num n t.update}


let mk ~id ~cost ~assignments ~patterns ~guard =
  let map_to_arg_vars =
    Enum.combine (List.enum patterns) (LazyList.enum Var.args)
    |> RenameMap.of_enum
  in
  let update =
    List.enum assignments
    |> Enum.map (Polynomial.rename map_to_arg_vars)
    |> Enum.combine (LazyList.enum Var.args)
    |> VarMap.of_enum
    |> fill_up_update_arg_vars_up_to_num (List.length patterns)
  in
  {
    id = if Option.is_none id then unique () else Option.get id;
    update;
    guard = Guard.rename guard map_to_arg_vars;
    invariant = Guard.mk_true;
    cost = Polynomial.rename map_to_arg_vars cost;
  }

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

let overapprox_nonlinear_updates t =
  let orig_guard_and_invariants = Guard.mk_and t.guard t.invariant in
  let overapprox_poly orig_var poly (guard, update) =
    if Polynomial.is_linear poly then guard, update
    else
      let handle_monom (coeff,mon) =
        if Monomial.is_univariate_linear mon then
          Guard.mk_true, Polynomial.mul (Polynomial.of_constant coeff) (Polynomial.of_monomial mon)
        else
          let new_var = Var.fresh_id Var.Int () in
          let new_var_poly = Polynomial.of_var new_var in
          let new_var_poly_with_coeff = Polynomial.mul (Polynomial.of_constant coeff) new_var_poly in
          (** check if update is quadratic, ^4, ^6, ... *)
          let vars = Monomial.vars mon in
          if VarSet.cardinal vars = 1 then
            let var = VarSet.any vars in
            if (Monomial.degree_variable var mon) mod 2 = 0 then
              let var_poly = Polynomial.of_var var in
              (* check if term will increase. drop nonlinear to ensure fast termination of SMT call *)
              let formula = Formula.mk_and
                              (Formula.mk_and  (Formula.mk_lt var_poly (Polynomial.of_int 2))
                                               (Formula.mk_gt var_poly (Polynomial.of_int (-2))))
                              (Formula.mk @@ Guard.drop_nonlinear orig_guard_and_invariants)
              in
              if SMT.Z3Solver.unsatisfiable formula then
                (* The update will increase the variable (disregarding factor) *)
                Guard.mk_and (Guard.mk_gt new_var_poly var_poly) (Guard.mk_ge new_var_poly (Polynomial.of_int 4)), new_var_poly_with_coeff
              else
                (* check if term can be zero. drop nonlinear to ensure fast termination of SMT call *)
                let formula = Formula.mk_and
                                (Formula.mk_eq var_poly (Polynomial.of_int 0))
                                (Formula.mk @@ Guard.drop_nonlinear orig_guard_and_invariants)
                in
                if SMT.Z3Solver.unsatisfiable formula then
                  (* updated variable will be positive (disregarding factor) *)
                  Guard.mk_and (Guard.mk_gt new_var_poly Polynomial.zero) (Guard.mk_ge new_var_poly var_poly), new_var_poly_with_coeff
                else
                  (* updated variable will be non-negative (disregarding factor) *)
                  Guard.mk_ge new_var_poly Polynomial.zero, new_var_poly_with_coeff
            else
              Guard.mk_true, new_var_poly_with_coeff
          else
            Guard.mk_true, new_var_poly_with_coeff
    in
    let (final_guard, final_upd_poly) =
      List.map handle_monom (Polynomial.monomials_with_coeffs poly)
      |> List.fold_left (fun (g,p) (g',p') -> Guard.mk_and g g', Polynomial.add p p') (guard, Polynomial.zero)
    in
    final_guard, VarMap.add orig_var final_upd_poly update
  in

  let (guard',update') = VarMap.fold overapprox_poly t.update (t.guard, t.update) in
  {t with guard = guard'; update = update'}

let eliminate_tmp_var var t =
  let orig_guard_and_invariants = Guard.mk_and t.guard t.invariant in
  let occurs_in_equality var label =
    let atoms = Guard.atom_list orig_guard_and_invariants in
    List.find_opt (fun (atom1,atom2) ->
        Atom.is_le atom1 &&
        Atom.is_le atom2 &&
        Atom.equal atom1 (Atom.flip_comp atom2) &&
        VarSet.mem var (Atom.vars atom1) &&
        Polynomial.var_only_linear var (Atom.poly atom1))
        (List.cartesian_product atoms atoms)
  in
  let opt = occurs_in_equality var t in
  if Option.is_some opt then
    let (atom1,atom2) = Option.get opt in
    let guard' = List.filter (fun atom -> not (Atom.equal atom1 atom || Atom.equal atom2 atom)) (Guard.atom_list @@ t.guard) in
    let inv' = List.filter (fun atom -> not (Atom.equal atom1 atom || Atom.equal atom2 atom)) (Invariant.atom_list @@ t.invariant) in
    let replacement =
      let coeff_of_var = Polynomial.coeff_of_indeterminate var (Atom.poly atom1) in
      if Z.(coeff_of_var < zero) then
        Polynomial.add (Atom.poly atom1) (Polynomial.of_coeff_list [Z.neg coeff_of_var] [var])
      else
        Polynomial.neg @@ Polynomial.add (Atom.poly atom1) (Polynomial.of_coeff_list [Z.neg coeff_of_var] [var])
    in
    let update' = VarMap.map (Polynomial.substitute var ~replacement:replacement) t.update in
    MaybeChanged.changed {t with guard = guard'; invariant = inv'; update = update'}
  else
    MaybeChanged.same t

let guard t = Guard.mk_and t.guard t.invariant

let chain_guards t1 t2 = guard (append t1 t2)

let guard_without_inv t = t.guard

let invariant t = t.invariant

let add_invariant t invariant' = {t with invariant = (Invariant.mk_and (t.invariant) invariant');}

let map_guard f label =
  { label with guard = f label.guard; invariant = f label.invariant}

let cost t = t.cost

let negative_costs t = SMT.Z3Solver.satisfiable Formula.(mk_and (mk @@ guard t) (mk_gt Polynomial.zero t.cost))

let only_update t = {t with cost = Polynomial.one; guard = Guard.mk_true; invariant = Invariant.mk_true}

let vars_ {update; guard; invariant; cost; _} =
  VarMap.fold (fun _ -> VarSet.union % Polynomial.vars) update VarSet.empty
  |> (VarSet.union % VarSet.of_enum % VarMap.keys) update
  |> (VarSet.union % Guard.vars) guard
  |> (VarSet.union % Invariant.vars) invariant
  |> (VarSet.union % Polynomial.vars) cost

(* TODO May invalidate through invariant generation! *)
let vars_memoization: (int,VarSet.t) Hashtbl.t = Hashtbl.create 10
let vars = Util.memoize vars_memoization ~extractor:id vars_
let vars_without_memoization = vars_ (** TODO remove this *)

let default = {
    id = 0;
    update = VarMap.empty;
    guard = Guard.mk_true;
    invariant = Invariant.mk_true;
    cost = Polynomial.one;
  }

let ids_to_string ?(pretty=false) t =
  if pretty then "t" ^ Util.natural_to_subscript (id t)
  else "t" ^ Int.to_string (id t)

let update_to_string update =
  if VarMap.is_empty update then
    "true"
  else
    update
    |> VarMap.bindings
    |> List.map (fun (var,poly) -> (Var.to_string var, Polynomial.to_string poly))
    |> List.split
    |> fun (xs,ys) -> "("^(String.concat "," xs)^") -> ("^(String.concat "," ys)^")"

let cost_to_string label =
  if
    Polynomial.is_one label.cost then ""
  else
    "{"^(Polynomial.to_string label.cost)^"}"

let normalise t (input_vars:VarSet.t) = {
    id = t.id;
    update = VarSet.fold (fun var fold_update -> if (VarMap.mem var fold_update)
                                            then fold_update
                                            else VarMap.add var (Polynomial.of_var var) fold_update) input_vars t.update;
    guard = t.guard;
    invariant = t.invariant;
    cost = t.cost;
  }

let update_to_string_lhs_ ?(to_file = false) t =
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

let update_to_string_lhs = update_to_string_lhs_ ~to_file:false
let update_to_file_string_lhs = update_to_string_lhs_ ~to_file:true

let update_to_string_rhs_ ?(to_file = false) t =
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

let update_to_string_rhs = update_to_string_rhs_ ~to_file:false
let update_to_file_string_rhs = update_to_string_rhs_ ~to_file:true

let update_to_string_lhs_pretty t =
  let update = t.update in
    if VarMap.is_empty update then
      ""
    else
      update
      |> VarMap.bindings
      |> List.map (fun (var,poly) -> (Var.to_string ~pretty:true var, (Polynomial.to_string_pretty poly)))
      |> List.split
      |> Tuple2.first
      |> fun xs -> "("^(String.concat ", " xs)^")"

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
      |> fun xs -> "("^(String.concat ", " xs)^")"

let to_string ?(pretty = false) t =
  let guard = if Guard.is_true t.guard  then "" else " :|: " ^ Guard.to_string ~pretty t.guard in
  let invariant = if Invariant.is_true t.invariant  then "" else " [" ^ Guard.to_string ~pretty t.invariant ^ "]" in
  let cost = if Polynomial.is_one t.cost then if pretty then "->" else "" else if pretty then "-{" ^ Polynomial.to_string_pretty t.cost else Polynomial.to_string t.cost ^ "}>" in
  if pretty then
    "t" ^ (Util.natural_to_subscript t.id) ^ ":" ^ invariant ^ " " ^ update_to_string_lhs_pretty t ^ " " ^ cost ^ " "  ^ update_to_string_rhs_pretty t ^ guard
  else
    "ID: " ^ invariant ^ string_of_int t.id ^ ", " ^ cost ^ "&euro;" ^ ", " ^ update_to_string t.update ^ guard

let to_id_string =
  string_of_int % id

let input_vars t =
  t.update
  |> VarMap.keys
  |> VarSet.of_enum

let has_tmp_vars t = not @@ VarSet.is_empty @@ VarSet.diff (vars t) (input_vars t)

let input_size t =
  t
  |> input_vars
  |> VarSet.cardinal

let tmp_vars t = VarSet.diff (vars t) (input_vars t)

let changed_vars t =
  input_vars t
  |> VarSet.filter (fun v -> not Polynomial.(equal (of_var v) (update t v |? of_var v)))

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

let rename rename_map t =
  {
    id = t.id;
    update = rename_update t.update rename_map;
    guard = Guard.rename t.guard rename_map;
    invariant = Invariant.rename t.invariant rename_map;
    cost = Polynomial.rename rename_map t.cost;
  }
  |> tap (fun _ -> Hashtbl.clear vars_memoization)

let relax_guard ?(non_static=VarSet.empty) t =
  let is_static atom = VarSet.subset (Atoms.Atom.vars atom) (VarSet.diff (input_vars t) non_static) in
  {t with guard = List.filter is_static t.guard}
let remove_non_contributors non_contributors t =
  let patterns = List.filter (flip VarSet.mem (VarSet.diff (input_vars t) non_contributors)) (VarSet.to_list @@ input_vars t) in
  let assignments = Util.cat_maybes @@ List.map (flip VarMap.find_opt t.update) patterns in
  mk ~cost:t.cost ~assignments:assignments ~patterns:patterns ~guard:t.guard ~id:None

(* We execute CFRefinement with guard && invariant -> We need to separate invariant afterwards. *)
let separate_guard_invariant t invariant' = {t with guard = List.filter (fun atom -> List.exists (fun atom_inv -> Atoms.Atom.equal atom atom_inv) invariant' |> not) t.guard;
                                                    invariant = List.filter (fun atom -> List.exists (fun atom_inv -> Atoms.Atom.equal atom atom_inv) invariant') t.guard}

