open Batteries
open Program
open Polynomials
open PolyExponential
open BoundsInst
open Constraints

let logger = Logging.(get Size)

type path = (Location.t * TWNLoop.t * Location.t) list

module Monomial = Monomials.Make(OurInt)

(** get_linear_update_of_variable (x<- 2x+3y+y^2) x y returns 3 *)
let get_linear_update_of_variable (t:TransitionLabel.t) (var_left:TWNLoop.VarMap.key) (var_right:TWNLoop.VarMap.key) =
  match (TransitionLabel.update t var_left) with
    | None -> (OurInt.of_int 0)
    | Some update ->
  let fupdate = List.filter (fun (x,y) -> Monomial.is_univariate_linear y) (Polynomial.monomials_with_coeffs update) in
  let (x,y) = List.find  (fun (x,y) -> (Var.equal (List.first (VarSet.to_list (Monomial.vars y))))  var_right) fupdate in
  x

(** get_linear_update_list [(x<- 2x+3y+y^2) x [x;y]] returns [[2;3]] *)
let rec get_linear_update_list (t:TransitionLabel.t) (var_left:TWNLoop.VarMap.key) (block:TWNLoop.VarMap.key list) = match block with
  | [] -> []
  | x::xs -> get_linear_update_of_variable t var_left x :: get_linear_update_list t var_left xs

let matrix_of_linear_assignments (t:TransitionLabel.t) (block:TWNLoop.VarMap.key list) =
  List.map (fun x -> get_linear_update_list t x block) block

let find_cycle appr program var (cycles: path list) =
    let f_eliminate t =
        EliminateNonContributors.eliminate_t (TWNLoop.vars t) (VarSet.singleton var) (TWNLoop.update t) TransitionLabel.remove_non_contributors (t |> TWNLoop.singleton |> TransitionLabel.only_update) in
    List.find_map (fun cycle ->
        let entries = Program.entry_transitions logger program (cycle |> List.map (Tuple3.map2 (List.first % TWNLoop.subsumed_transitionlabels))) in
        let twn_loops = List.map (fun (_,_,l') -> TWN.compose_transitions cycle (TWN.find l' cycle) |> f_eliminate) entries in
      let blocks = List.map (fun twn_loop ->
          let block = TWN.check_solvable_t twn_loop in
          if Option.is_some block then
            let unwrapped_block = Option.get block |> List.find (List.mem var) in
            if List.for_all (Polynomial.is_linear % (TransitionLabel.update_full twn_loop)) unwrapped_block then
              let update_matrix = matrix_of_linear_assignments twn_loop unwrapped_block in
              Some (update_matrix |> List.flatten, unwrapped_block)
            else
              None
          else
            None) twn_loops in
    if List.for_all Option.is_some blocks then
      Option.some (List.map Option.get blocks |> List.combine entries)
    else
      None) cycles

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
  let command = "python3 -c 'from src.bounds.SizeBoundSolvable import size_bound; size_bound(" ^ update_str ^ "," ^ vars_str ^ ",\"" ^ Var.to_string var ^ "\")'" in
  let python_output = read_process_lines command in
  match python_output with
      | [a] -> Some (a |> Readers.read_bound)
      | _ -> None (*error string *)

let improve_t program trans (l,t,l') appr =
  VarSet.fold (fun var appr ->
  if Approximation.sizebound appr (l,t,l') var |> Bound.is_linear |> not then
      try (
      let cycle = find_cycle appr program var [[(l,TWNLoop.mk_transition t,l')]] in
      List.map (fun (entry, (update_as_matrix, block)) ->
          run_python var block update_as_matrix |? Bound.infinity
            |> Bound.substitute (Var.of_string "n") ~replacement:(Approximation.timebound appr (l,t,l'))) cycle
      |> Bound.sum_list
      |> (fun bound -> Approximation.add_sizebound bound (l,t,l') var appr))
      with Not_found ->  appr
  else appr) (TransitionLabel.input_vars t) appr

let improve program ?(scc = None) appr =
    let trans = (if Option.is_some scc then (Option.get scc) else Program.transitions program)
        |> TransitionSet.to_list
        |> List.filter (fun (l,t,l') -> Approximation.is_time_bounded appr (l,t,l')) in
    List.fold_right (improve_t program trans) trans appr
