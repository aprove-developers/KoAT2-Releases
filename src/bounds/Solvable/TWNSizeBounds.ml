
open Batteries
open BoundsInst
open Constraints
open Polynomials
open PolyExponential
open ProgramModules

module Check_TWN = Check_TWN.Make(ProgramModules)
module Loop = Loop.Make(ProgramModules)
module SimpleCycle = SimpleCycle.Make(ProgramModules)

let heuristic_for_cycle appr program loop = Check_TWN.check_twn loop

module VT = struct
  type t = Transition.t * Var.t

  let equal (t1,v1) (t2,v2) = Transition.equal t1 t2 && Var.equal v1 v2

  let hash = Hashtbl.hash
end

module VarMap = Map.Make(Var)

module SizeBoundTable = Hashtbl.Make(VT)

let size_bound_table: (PE.t Option.t * (Transition.t * Polynomial.t VarMap.t) list) SizeBoundTable.t = SizeBoundTable.create 10

let lift appr t var closed_form (entry,traversal) =
  (* Insert runtime bound. *)
  let local_size =
    if Option.is_some closed_form then
      PE.overapprox (Option.get closed_form) (Approximation.timebound appr t)
      |> Bound.substitute_f (fun var -> Bound.of_poly @@ (VarMap.find_opt var traversal |? Polynomial.of_var var))
    else
      Bound.infinity
  in
  Bound.substitute_f (Approximation.sizebound appr entry) local_size

let improve_t program trans t appr =
  VarSet.fold (fun var appr ->
    if SizeBoundTable.mem size_bound_table (t,var) then
      let closed_form,entry_traversal = SizeBoundTable.find size_bound_table (t,var) in
      let lifted_bound = List.map (lift appr t var closed_form) entry_traversal |> List.enum |> Bound.sum in
      Approximation.add_sizebound lifted_bound t var appr
    else
      if not @@ Bound.is_polynomial @@ Approximation.sizebound appr t var then
        let loops_opt = SimpleCycle.find_loop ~relevant_vars:(Option.some @@ VarSet.singleton var) heuristic_for_cycle appr program trans t in
        if Option.is_some loops_opt then
          let loop, entries_traversal = Option.get loops_opt in
          let local_bound =
              (* We first compute for var (with a closed form) a local size bound *)
              let order = Check_TWN.check_triangular loop in
              if List.is_empty order then None
              else
                let closed_form =
                    PE.compute_closed_form @@ List.map (fun var ->
                        (var, Loop.update_var loop var)) order
                    |> List.combine order
                    |> List.find (Var.equal var % Tuple2.first)
                    |> Tuple2.second
                in
                Option.some closed_form
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

