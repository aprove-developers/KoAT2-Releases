open Batteries

module TransitionSet = Set.Make(Program.Transition)
             
(* Returns the maximum of all incoming sizebounds applicated to the local sizebound.
       Corresponds to 'SizeBounds for trivial SCCs':
       S'(alpha) = max{ S_l(alpha)(S(t',v_1),...,S(t',v_n)) | t' in pre(t) } *)
let highest_incoming_bound (program: Program.t)
                           (appr: Approximation.t)
                           (local_sizebound: Bound.t)
                           (t: Program.Transition.t)
    : Bound.t =
  let substitute_with_prevalues t' = Bound.substitute_f Approximation.(sizebound Upper appr t') local_sizebound in
  Bound.maximum (List.map substitute_with_prevalues (TransitionSet.to_list (Program.pre program t)))

(* Improves a trivial scc. That is an scc which consists only of one result variable.
       Corresponds to 'SizeBounds for trivial SCCs'. *)
let improve_trivial_scc (program: Program.t)
                        (appr: Approximation.t)
                        (t,v)
    : Approximation.t =
  let (local_sizebound: Bound.t) = TransitionLabel.(LocalSizeBound.(as_bound (sizebound_local Upper (Program.Transition.label t) v))) in
  let newbound =
    if Program.is_initial program t then
      local_sizebound
    else highest_incoming_bound program appr local_sizebound t
  in Approximation.(add_sizebound Upper newbound t v appr)      
   
(* Improves a nontrivial scc. That is an scc which consists of more than one result variable.
       Corresponds to 'SizeBounds for nontrivial SCCs'. *)
let improve_nontrivial_scc (program: Program.t)
                           (rvg: Program.RVG.t)
                           (appr: Approximation.t)
                           (scc: Program.RV.t list)
    : Approximation.t =
  raise (Failure "Not yet implemented")

(* Improves a whole scc. *)
let improve_scc (program: Program.t)
                (rvg: Program.RVG.t)
                (appr: Approximation.t)
                (scc: Program.RV.t list)
    : Approximation.t  =
  match scc with
  | [rv] -> improve_trivial_scc program appr rv
  | scc -> improve_nontrivial_scc program rvg appr scc
         
let improve program appr =
  let module C = Graph.Components.Make(Program.RVG) in
  let rvg = Program.rvg program in
  List.fold_left (fun appr scc -> improve_scc program rvg appr scc) appr (C.scc_list rvg)
