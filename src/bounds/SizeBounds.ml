open Batteries

module Make(A : BoundTypes.Approximation) =
  struct

    module Approximation_ = A
    module Program_ = Approximation_.Program_
    module RVG = Approximation_.Program_.RVG
    module Bound = Approximation_.Bound
               
    module VarMap = Map.Make(Approximation_.Bound.Var)

    type classification = Equality | Constant | ScaledSum | Unbound

    let classify (poly: Approximation_.Bound.t): classification =
      raise (Failure "Not yet implemented")

    (* Returns a list of all transitions which occur directly before the given transition in the graph. 
       Corresponds to pre(t). *)
    let pre (program: Program_.t)
            ((l,t,l'): Program_.Transition.t)
        : Program_.Transition.t list =
      Program_.(TransitionGraph.pred_e (graph program) l)

    (* Returns the maximum of all incoming sizebounds applicated to the local sizebound.
       Corresponds to 'SizeBounds for trivial SCCs':
       S'(alpha) = max{ S_l(alpha)(S(t',v_1),...,S(t',v_n)) | t' in pre(t) } *)
    let highest_incoming_bound (program: Program_.t)
                               (appr: Approximation_.t)
                               (local_sizebound: Bound.t)
                               (t: Program_.Transition.t)
      : Bound.t =
      let substitute_with_prevalues t' = Bound.substitute_f Approximation_.(sizebound Upper appr t') local_sizebound in
      Bound.maximum (List.map substitute_with_prevalues (pre program t))

    (* Improves a trivial scc. That is an scc which consists only of one result variable.
       Corresponds to 'SizeBounds for trivial SCCs'. *)
    let improve_trivial_scc (program: Program_.t)
                            (appr: Approximation_.t)
                            (t,v)
        : Approximation_.t =
      let (local_sizebound: Bound.t) = Approximation_.(sizebound_local Upper appr t v) in
      let newbound =
        if Program_.is_initial program t then
          local_sizebound
        else highest_incoming_bound program appr local_sizebound t
      in Approximation_.(add_sizebound Upper newbound t v appr)      
      
    (* Improves a nontrivial scc. That is an scc which consists of more than one result variable.
       Corresponds to 'SizeBounds for nontrivial SCCs'. *)
    let improve_nontrivial_scc (program: Program_.t)
                               (rvg: RVG.t)
                               (appr: Approximation_.t)
                               (scc: RVG.V.t list)
        : Approximation_.t =
      raise (Failure "Not yet implemented")

    (* Improves a whole scc. *)
    let improve_scc (program: Program_.t)
                    (rvg: RVG.t)
                    (appr: Approximation_.t)
                    (scc: RVG.V.t list)
        : Approximation_.t  =
      match scc with
      | [rv] -> improve_trivial_scc program appr rv
      | scc -> improve_nontrivial_scc program rvg appr scc
      
    let improve program appr =
      let module C = Graph.Components.Make(RVG) in
      let rvg = Program_.rvg program in
      List.fold_left (fun appr scc -> improve_scc program rvg appr scc) appr (C.scc_list rvg)
      
  end
