
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

let size_bound_table: (Transition.t * Bound.t) list option SizeBoundTable.t = SizeBoundTable.create 10

(** Internal memoization: The idea is to use this cache if we applied cfr and
  1) delete it and use the original cache if we get a timeout or
  2) if the analysis of the unrolled scc is completed successfully use this cache as the main memory.
  TODO Currently, we just reset the cache. *)
let reset_cfr () =
  SizeBoundTable.clear size_bound_table

let lift appr = function
  | None -> Bound.infinity
  | Some xs -> xs
               |> List.map (fun (entry,local_size) -> Bound.substitute_f (Approximation.sizebound appr entry) local_size)
               |> List.enum
               |> Bound.sum

module TWN_Complexity = TWN_Complexity.Make(ProgramModules)
let compute_time_bound loop =
  let mprf_bound = MultiphaseRankingFunction.time_bound loop 5 in
  if Bound.is_infinity mprf_bound && Check_TWN.check_twn loop then
    TWN_Complexity.complexity ~termination:false [] loop
  else
    mprf_bound

let improve_t program trans t appr =
  VarSet.fold (fun var appr ->
    if Polynomial.is_linear (TransitionLabel.update (Transition.label t) var |? Polynomial.of_var var) then
      appr
    else
      if SizeBoundTable.mem size_bound_table (t,var) then
        let lifted_bound = lift appr (SizeBoundTable.find size_bound_table (t,var)) in
        Approximation.add_sizebound lifted_bound t var appr
      else
        if not @@ Bound.is_polynomial @@ Approximation.sizebound appr t var then
          let loops_opt = SimpleCycle.find_loop heuristic_for_cycle ~relevant_vars:(Option.some @@ VarSet.singleton var) appr program trans t in
          if Option.is_some loops_opt then
            let loop, entries_traversal = Option.get loops_opt in
            let local_bound =
                let loop_red = Loop.eliminate_non_contributors ~relevant_vars:(Option.some @@ VarSet.singleton var) loop in
                (* We first compute for var (with a closed form) a local size bound *)
                let order = Check_TWN.check_triangular loop_red in
                if List.is_empty order then None
                else
                  let closed_form =
                      PE.compute_closed_form @@ List.map (fun var ->
                          (var, Loop.update_var loop_red var)) order
                      |> List.combine order
                      |> List.find (Var.equal var % Tuple2.first)
                      |> Tuple2.second
                  in
                  let time_bound = compute_time_bound loop in (* We assume that loop terminates (since t terminates). *)
                  List.map (fun (entry,traversal) -> entry,
                    PE.overapprox closed_form time_bound
                    |> Bound.substitute_f (fun var -> Bound.of_poly @@ (VarMap.find_opt var traversal |? Polynomial.of_var var))) entries_traversal
                    |> Option.some
            in
            SizeBoundTable.add size_bound_table (t,var) local_bound;
              (* Lifting previously computed local size bounds and store them in appr. *)
            let lifted_bound = lift appr local_bound in
            Approximation.add_sizebound lifted_bound t var appr
        else
          appr
      else
        appr) (TransitionLabel.input_vars (Transition.label t)) appr

let improve program ?(scc = None) appr =
  let trans = scc |? TransitionSet.filter (Approximation.is_time_bounded appr) @@ Program.transitions program in
  TransitionSet.fold (improve_t program trans) trans appr

