open Batteries
open ProgramTypes
open Polynomials
open PolyExponential
open BoundsInst
open Constraints

let logger = Logging.(get Size)

let compute_order t var =
    let t' = EliminateNonContributors.eliminate_t (TransitionLabel.vars t) (VarSet.singleton var) (TransitionLabel.update t) TransitionLabel.remove_non_contributors (t |> TransitionLabel.uniform) in
    TWN.check_triangular_t t'

let improve_t program (l,t,l') appr =
    let entries = Program.entry_transitions logger program [(l,t,l')] in
    VarSet.fold (fun var appr ->
    if Approximation.sizebound appr (l,t,l') var |> Bound.is_infinity then
        let order = compute_order t var in
        let closed_form = PE.compute_closed_form (List.map (fun var ->
            let update_var = TransitionLabel.update t var in
            (var, if Option.is_some update_var then Option.get update_var else Polynomial.of_var var)) order) in
        if List.is_empty closed_form |> not then
            List.fold_right (fun (var, pe) appr ->
                if Approximation.sizebound appr (l,t,l') var |> Bound.is_finite |> not then
                    let local_bound = PE.overapprox pe (Approximation.timebound appr (l,t,l')) in
                    let bound = List.map (fun t -> Bound.substitute_f (fun var -> Approximation.sizebound appr t var) local_bound) entries |> Bound.sum_list in
                    Approximation.add_sizebound bound (l,t,l') var appr
                else appr) (List.combine order closed_form) appr
        else
            appr
    else appr) (TransitionLabel.vars t) appr

let improve program ?(scc = None) appr =
    let trans = (if Option.is_some scc then (Option.get scc) else Program.transitions program)
        |> TransitionSet.to_list
        |> List.filter (fun (l,t,l') -> Approximation.is_time_bounded appr (l,t,l') && Location.equal l l') in
    List.fold_right (improve_t program) trans appr
