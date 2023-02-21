open Batteries
open ProgramTypes
open ProgramModules
open Polynomials
open PolyExponential
open BoundsInst
open Constraints

let logger = Logging.(get Size)

module Check_Solvable = Check_Solvable.Make(ProgramModules)
module Loop = Loop.Make(ProgramModules)
module SimpleCycle = SimpleCycle.Make(ProgramModules)

module Monomial = Monomials.Make(OurInt)
module ScaledMonomials = ScaledMonomials.Make(OurInt)

(** get_linear_update_of_variable (x<- 2x+3y+y^2) x y returns 3 *)
let get_factor_of_var t update (var: Var.t) =
  let linear_monomials = List.filter (Monomial.is_univariate_linear % ScaledMonomials.monomial) (Polynomial.scaled_monomials update) in
  let linear_monomials_var = List.filter (VarSet.mem var % ScaledMonomials.vars) linear_monomials in
  List.map ScaledMonomials.coeff linear_monomials_var
  |> OurInt.sum_list

(** get_linear_update_list (x<- 2x+3y+y^2) x [x;y] returns [2;3] *)
let rec get_linear_update_of_var t (block: Var.t list) (var_left: Var.t) = match block with
  | [] -> []
  | x::xs ->
    (get_factor_of_var t (TransitionLabel.update t var_left |? Polynomial.of_var var_left) x) ::
    (get_linear_update_of_var t xs var_left)

let matrix_of_linear_assignments t (block: Var.t list) =
  List.map (get_linear_update_of_var t block) block

(* This function was written by Tom Küspert *)
let read_process_lines command =
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

let run_python var block update_matrix =
  let vars_str = (* vars x1,x2,... are encoded (for the python script) by ["x1","x2",...]. *)
    block
    |> List.map (fun var -> "\"" ^ (Var.to_string var) ^ "\"")
    |> List.enum
    |> Util.enum_to_string identity
    |> Str.global_replace (Str.regexp ";") "," and
  update_str = (* update matrix A encoded (for the python script) row-wise w.r.t. x1,x2,.... *)
    update_matrix
    |> List.flatten
    |> List.enum
    |> Util.enum_to_string OurInt.to_string
    |> Str.global_replace (Str.regexp ";") "," in
  let command = "python3 -c 'from src.bounds.Solvable.SizeBoundSolvable import size_bound; size_bound(" ^ update_str ^ "," ^ vars_str ^ ",\"" ^ Var.to_string var ^ "\")'" in
  let python_output = read_process_lines command in
  match python_output with
      | [a] -> Some (a |> Readers.read_bound)
      | _ -> None (* Error string *)

let heuristic_for_cycle appr program loop =
  Option.is_some @@ Check_Solvable.check_solvable loop
  && VarSet.for_all (fun var -> Polynomial.is_linear @@ Loop.update_var loop var) (Loop.updated_vars loop)
  && VarSet.for_all (fun var -> Polynomial.no_constant_addend @@ Loop.update_var loop var) (Loop.updated_vars loop)

module VT = struct
  type t = Transition.t * Var.t

  let equal (t1,v1) (t2,v2) = Transition.equal t1 t2 && Var.equal v1 v2

  let hash = Hashtbl.hash
end

module SizeBoundTable = Hashtbl.Make(VT)

let size_bound_table: (Bound.t * (Transition.t * Polynomial.t VarMap.t) list) SizeBoundTable.t = SizeBoundTable.create 10

let lift appr t var bound (entry,traversal) =
  (* Insert runtime bound. *)
  let local_size = Bound.substitute (Var.of_string "n") ~replacement:(Approximation.timebound appr t) bound
  |> Bound.substitute_f (fun var -> Bound.of_poly @@ (VarMap.find_opt var traversal |? Polynomial.of_var var)) in
  Bound.substitute_f (fun var -> Approximation.sizebound appr entry var) local_size

let improve_t program trans t appr =
  VarSet.fold (fun var appr ->
    if SizeBoundTable.mem size_bound_table (t, var) then
      let local_bound,entry_traversal = SizeBoundTable.find size_bound_table (t, var) in
      let lifted_bound = List.map (lift appr t var local_bound) entry_traversal |> List.enum |> Bound.sum in
      Approximation.add_sizebound lifted_bound t var appr
    else
      if not @@ Bound.is_polynomial @@ Approximation.sizebound appr t var then
        let loops_opt = SimpleCycle.find_loop ~relevant_vars:(Option.some @@ VarSet.singleton var) heuristic_for_cycle appr program trans t in
        if Option.is_some loops_opt then
          let loop, entries_traversal = Option.get loops_opt in
          let local_bound =
            (* We first compute for every var (with a closed form) and every entry a local size bound *)
              let blocks = Check_Solvable.check_solvable loop in
              if Option.is_none blocks then Bound.infinity
              else
                let block = List.find (List.mem var) @@ Option.get blocks in
                run_python var block (matrix_of_linear_assignments (Transition.label t) block) |? Bound.infinity
          in
          SizeBoundTable.add size_bound_table (t,var) (local_bound,entries_traversal);
          (* Lifting previously computed local size bounds and store them in appr. *)
          let lifted_bound = List.map (lift appr t var local_bound) entries_traversal |> List.enum |> Bound.sum in
          Approximation.add_sizebound lifted_bound t var appr
        else
          appr
      else
        appr) (TransitionLabel.input_vars (Transition.label t)) appr

let improve program ?(scc = None) appr =
  let trans = scc |? TransitionSet.filter (Approximation.is_time_bounded appr) @@ Program.transitions program in
  TransitionSet.fold (improve_t program trans) trans appr
