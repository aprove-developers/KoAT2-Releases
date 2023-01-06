open Batteries
open BoundsInst
open Constraints
open Polynomials
open PolyExponential
open ProgramModules

let logger = Logging.(get Size)

module Check_TWN = Check_TWN.Make(ProgramModules)
module TWN = TWN.Make(ProgramModules)
module TWNLoop = TWNLoop.Make(ProgramModules)
module Loop = SimpleCycle.Loop(ProgramModules)
module SimpleCycle = SimpleCycle.SimpleCycle(ProgramModules)

let compute_order t var =
    let t' = EliminateNonContributors.eliminate_t (TransitionLabel.vars t) (VarSet.singleton var) (TransitionLabel.update t) TransitionLabel.remove_non_contributors (t |> TransitionLabel.only_update) in
    Check_TWN.check_triangular_t t'

let heuristic_for_cycle appr entry program loop =
    Check_TWN.check_twn_loop loop && VarSet.for_all (Bound.is_finite % Approximation.sizebound appr entry) (Program.input_vars program)

let lift appr var (entry,local_bound) =
    Bound.substitute_f (fun var -> Approximation.sizebound appr entry var) local_bound

let improve_t program trans (l,t,l') appr =
    VarSet.fold (fun var appr ->
        if Approximation.sizebound appr (l,t,l') var |> Bound.is_infinity then
            let loops_opt = SimpleCycle.find_loops ~relevant_vars:(Option.some @@ VarSet.singleton var) heuristic_for_cycle appr program trans t in
            if Option.is_some loops_opt then
                let cycle, loops = Option.get loops_opt in
                let local_bounds =
                    (* We first compute for every var (with a closed form) and every entry a local size bound *)
                    List.map (fun (entry,loop) ->
                        let order = Check_TWN.check_triangular loop in
                        if List.is_empty order then ((l,t,l'),Bound.infinity)
                        else
                            let closed_form =
                                PE.compute_closed_form @@ List.map (fun var ->
                                    (var, Loop.update_var loop var)) order
                                |> List.combine order
                                |> List.find (Var.equal var % Tuple2.first)
                                |> Tuple2.second
                            in
                            entry, PE.overapprox closed_form (Approximation.timebound appr (l,t,l'))) loops
                in
                (* Lifting previously computed local size bounds and store them in appr. *)
                let lifted_bound = List.map (lift appr var) local_bounds |> List.enum |> Bound.sum in
                List.fold_right (fun t -> Approximation.add_sizebound lifted_bound t var) cycle appr
            else
                appr
        else
            appr) (TransitionLabel.input_vars t) appr

let improve program ?(scc = None) appr =
    let trans = (if Option.is_some scc then (Option.get scc) else Program.transitions program)
        |> TransitionSet.filter (fun (l,t,l') -> Approximation.is_time_bounded appr (l,t,l')) in
    TransitionSet.fold (improve_t program trans) trans appr
