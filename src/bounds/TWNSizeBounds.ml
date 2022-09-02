open Batteries
open ProgramTypes
open Polynomials
open PolyExponential
open BoundsInst
open Constraints

let logger = Logging.(get Size)

let compute_order t var =
    let t' = EliminateNonContributors.eliminate_t (TransitionLabel.vars t) (VarSet.singleton var) (TransitionLabel.update t) TransitionLabel.remove_non_contributors (t |> TransitionLabel.only_update) in
    TWN.check_triangular_t t'

type path = (Location.t * TWNLoop.t * Location.t) list

let find_cycle appr program var (cycles: path list) =
    let f_eliminate t =
        EliminateNonContributors.eliminate_t (TWNLoop.vars t) (VarSet.singleton var) (TWNLoop.update t) TransitionLabel.remove_non_contributors (t |> TWNLoop.singleton |> TransitionLabel.only_update) in
    List.find_map (fun cycle ->
        let entries = Program.entry_transitions logger program (cycle |> List.map (Tuple3.map2 (List.first % TWNLoop.subsumed_transitionlabels))) in
        let twn_loops = List.map (fun (_,_,l') -> TWN.compose_transitions cycle (TWN.find l' cycle) |> f_eliminate) entries in
        if List.for_all (fun t -> TWN.check_triangular_t t != []) twn_loops then
            Option.some (List.combine twn_loops entries)
        else
            None) cycles


let improve_t program trans (l,t,l') appr =
    let entries = Program.entry_transitions logger program [(l,t,l')] in
    VarSet.fold (fun var appr ->
    if Approximation.sizebound appr (l,t,l') var |> Bound.is_infinity then
        let parallel_edges = TWN.parallel_edges [] trans in
        let cycle = find_cycle appr program var (
          if Location.equal l l' then
            let f (l1,loop,l1') = Location.equal l l1 && Location.equal l' l1' && String.equal (TransitionLabel.update_to_string_rhs t) (TWNLoop.update_to_string_rhs loop) in
            [[List.find f parallel_edges]]
          else
            (TWN.cycles (parallel_edges |> Set.of_list) l [([(l,TWNLoop.mk_transition t,l')], (LocationSet.singleton l'))] []))
        in
        List.map (fun (cycle, entry) ->
            let order = TWN.check_triangular_t cycle in
            if List.is_empty order then Bound.infinity
            else
                let closed_form = PE.compute_closed_form (List.map (fun var ->
                let update_var = TransitionLabel.update t var in
                    (var, if Option.is_some update_var then Option.get update_var else Polynomial.of_var var)) order) in
                if List.is_empty closed_form |> not then
                    List.map (fun (var, pe) ->
                            let local_bound = PE.overapprox pe (Approximation.timebound appr (l,t,l')) in
                            Bound.substitute_f (fun var -> Approximation.sizebound appr entry var) local_bound)
                        (List.combine order closed_form)
                        |> Bound.sum_list
                else
                    Bound.infinity) cycle
        |> Bound.sum_list
        |> (fun bound -> Approximation.add_sizebound bound (l,t,l') var appr)
    else appr) (TransitionLabel.input_vars t) appr

let improve program ?(scc = None) appr =
    let trans = (if Option.is_some scc then (Option.get scc) else Program.transitions program)
        |> TransitionSet.to_list
        |> List.filter (fun (l,t,l') -> Approximation.is_time_bounded appr (l,t,l')) in
    List.fold_right (improve_t program trans) trans appr
