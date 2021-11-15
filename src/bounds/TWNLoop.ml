open Batteries
open Polynomials

module Guard = Formulas.Formula
module Invariant = Formulas.Formula
type polynomial = Polynomial.t
module VarMap = Map.Make(Var)

module TWNLoop = struct

  type t = {
    id : int;
    update : Polynomial.t VarMap.t;
    guard : Guard.t;
    invariant :Invariant.t;
    cost : Polynomial.t;
  }

  let id t = t.id

  let update t var = VarMap.Exceptionless.find var t.update

  let guard t = t.guard

  let invariant t = t.invariant

  let cost t = t.cost

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


  let update_to_string update =
  if VarMap.is_empty update then
    "true"
  else
    update
    |> VarMap.bindings
    |> List.map (fun (var,poly) -> (Var.to_string var, Polynomial.to_string poly))
    |> List.split
    |> fun (xs,ys) -> "("^(String.concat "," xs)^") -> ("^(String.concat "," ys)^")"

let cost_to_string  label =
  if
    Polynomial.is_one label.cost then ""
  else
    "{"^(Polynomial.to_string label.cost)^"}"


let guard_to_string ?(pretty = false) label =
  if
    Guard.is_true (Guard.mk_and label.guard label.invariant) then ""
  else
    Guard.to_string ~pretty (Guard.mk_and label.guard label.invariant)

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

let normalise t (input_vars:VarSet.t) = {
    id = t.id;
    update = VarSet.fold (fun var fold_update -> if (VarMap.mem var fold_update)
                                            then fold_update
                                            else VarMap.add var (Polynomial.of_var var) fold_update) input_vars t.update;
    guard = t.guard;
    invariant = t.invariant;
    cost = t.cost;
  }

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
  let invariant = if Invariant.is_true label.invariant  then "" else " [" ^ invariant_to_string ~pretty label ^ "]" in
  let cost = if Polynomial.is_one label.cost then if pretty then "->" else "" else if pretty then "-{" ^ Polynomial.to_string_pretty label.cost else Polynomial.to_string label.cost ^ "}>" in
  if pretty then
    "t" ^ (Util.natural_to_subscript label.id) ^ ":" ^ invariant ^ " " ^ update_to_string_lhs_pretty label ^ " " ^ cost ^ " "  ^ update_to_string_rhs_pretty label ^ guard
  else 
    "ID: " ^ invariant ^ string_of_int label.id ^ ", " ^ cost ^ "&euro;" ^ ", " ^ update_to_string label.update ^ guard

let to_id_string =
  string_of_int % id
end