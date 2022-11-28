open Batteries
open ProgramTypes
open ProgramModules
open Polynomials
open PolyExponential
open BoundsInst
open Constraints

let logger = Logging.(get Size)

type path = (Location.t * TWNLoop.t * Location.t) list

module Monomial = Monomials.Make(OurInt)

(** get_linear_update_of_variable (x<- 2x+3y+y^2) x y returns 3 *)
let get_linear_update_of_variable (t:TransitionLabel.t) (var_left:VarMap.key) (var_right:VarMap.key) =
  match (TransitionLabel.update t var_left) with
    | None -> (OurInt.of_int 0)
    | Some update ->
  let fupdate = List.filter (fun (x,y) -> Monomial.is_univariate_linear y) (Polynomial.monomials_with_coeffs update) in
  let (x,y) = List.find  (fun (x,y) -> (Var.equal (List.first (VarSet.to_list (Monomial.vars y))))  var_right) fupdate in
  x

(** get_linear_update_list [(x<- 2x+3y+y^2) x [x;y]] returns [[2;3]] *)
let rec get_linear_update_list (t:TransitionLabel.t) (var_left:VarMap.key) (block:VarMap.key list) = match block with
  | [] -> []
  | x::xs -> get_linear_update_of_variable t var_left x :: get_linear_update_list t var_left xs

let matrix_of_linear_assignments (t:TransitionLabel.t) (block:VarMap.key list) =
  List.map (fun x -> get_linear_update_list t x block) block

let find_cycle twn_loop var =
  let f_eliminate t =
    EliminateNonContributors.eliminate_t (TransitionLabel.vars t) (VarSet.singleton var) (TransitionLabel.update t) TransitionLabel.remove_non_contributors (TransitionLabel.only_update t) in
  let block = Check_Solvable.check_solvable_t (f_eliminate twn_loop) in
    if Option.is_some block then
      let unwrapped_block = Option.get block |> List.find (List.mem var) in
      if List.for_all (Polynomial.is_linear % (TransitionLabel.update_full twn_loop)) unwrapped_block then
        let update_matrix = matrix_of_linear_assignments twn_loop unwrapped_block in
        Some (update_matrix |> List.flatten, unwrapped_block)
      else
        None
    else
      None

let read_process_lines command = (* This function was written by Tom Küspert *)
  let lines = ref [] in
  let in_channel = Unix.open_process_in command in
  begin
    try
      while true do
        lines := input_line in_channel :: !lines
      done;
    with
      | BatInnerIO.Input_closed -> ()
      | End_of_file -> ()
  end;
  List.rev !lines

let rec replace (char: char) (replacement: char) (str: char list) = match str with
  | x::xs when x = char -> replacement::(replace char replacement str)
  | x::xs -> x::(replace char replacement str)
  | [] -> []

let run_python var block update =
  let vars_str = block |> List.map (fun var -> "\"" ^ (Var.to_string var) ^ "\"") |> List.enum |> Util.enum_to_string identity |> Str.global_replace (Str.regexp ";") "," and
  update_str = update |> List.enum |> Util.enum_to_string OurInt.to_string |> Str.global_replace (Str.regexp ";") "," in
  let command = "python3 -c 'from src.bounds.Solvable.SizeBoundSolvable import size_bound; size_bound(" ^ update_str ^ "," ^ vars_str ^ ",\"" ^ Var.to_string var ^ "\")'" in
  let python_output = read_process_lines command in
  match python_output with
      | [a] -> Some (a |> Readers.read_bound)
      | _ -> None (*error string *)

let improve_t program trans (l,t,l') appr =
  VarSet.fold (fun var appr ->
  if Approximation.sizebound appr (l,t,l') var |> Bound.is_linear |> not then
    let entries = Program.entry_transitions logger program [(l,t,l')] in
    let block = find_cycle t var in
    if Option.is_some block then
      let (update_as_matrix, block) =
        Option.get block
      in
      List.map (fun entry ->
        run_python var block update_as_matrix |? Bound.infinity
        |> Bound.substitute (Var.of_string "n") ~replacement:(Approximation.timebound appr (l,t,l'))) entries
      |> Bound.sum_list
      |> (fun bound -> Approximation.add_sizebound bound (l,t,l') var appr)
    else
      appr
  else appr) (TransitionLabel.input_vars t) appr

let improve program ?(scc = None) appr =
    let trans = (if Option.is_some scc then (Option.get scc) else Program.transitions program)
        |> TransitionSet.to_list
        |> List.filter (fun (l,t,l') -> Approximation.is_time_bounded appr (l,t,l')) in
    List.fold_right (improve_t program trans) trans appr
