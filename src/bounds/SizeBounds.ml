open Batteries

module Make(A : BoundTypes.Approximation) =
  struct

    module Approximation_ = A
    module TransitionGraph_ = Approximation_.TransitionGraph_
    module RVG = Approximation_.TransitionGraph_.RVG

    module VarMap = Map.Make(Approximation_.Bound.Var)

    type classification = Equality | Constant | ScaledSum | Unbound

    let classify (poly: Approximation_.Bound.t): classification =
      raise (Failure "Not yet implemented")

    (* Returns a list of all transitions which occur directly before the given transition in the graph. 
       Corresponds to pre(t). *)
    let pre (transitiongraph: TransitionGraph_.t)
            (l: TransitionGraph_.TransitionGraph.V.t)
        : TransitionGraph_.TransitionGraph.E.t list =
      TransitionGraph_.(TransitionGraph.pred_e (graph transitiongraph) l)

    (* Returns the maximum of all incoming sizebounds applicated to the local sizebound.
       Corresponds to 'SizeBounds for trivial SCCs':
       S'(alpha) = max{ S_l(alpha)(S(t',v_1),...,S(t',v_n)) | t' in pre(t) } *)
    let highest_incoming_bound (transitiongraph: TransitionGraph_.t)
                               (appr: Approximation_.t)
                               (local_sizebound: Approximation_.Bound.t)
                               (l: TransitionGraph_.TransitionGraph.V.t)
      : Approximation_.Bound.t =
      let substitute_with_prevalues (_,t',_) = Approximation_.Bound.substitute_f Approximation_.(sizebound Upper appr t') local_sizebound in
      Approximation_.Bound.maximum (List.map substitute_with_prevalues (pre transitiongraph l))

    (* Improves a trivial scc. That is an scc which consists only of one result variable.
       Corresponds to 'SizeBounds for trivial SCCs'. *)
    let improve_trivial_scc (transitiongraph: TransitionGraph_.t)
                            (appr: Approximation_.t)
                            ((l,t,l'),v)
        : Approximation_.t =
      let (local_sizebound: Approximation_.Bound.t) = Approximation_.(sizebound_local Upper appr t v) in
      let newbound =
        if TransitionGraph_.is_initial transitiongraph t then
          local_sizebound
        else highest_incoming_bound transitiongraph appr local_sizebound l
      in Approximation_.(add_sizebound Upper newbound t v appr)      
      
    (* Improves a nontrivial scc. That is an scc which consists of more than one result variable.
       Corresponds to 'SizeBounds for nontrivial SCCs'. *)
    let improve_nontrivial_scc (transitiongraph: TransitionGraph_.t)
                               (variablegraph: RVG.t)
                               (appr: Approximation_.t)
                               (scc: RVG.V.t list)
        : Approximation_.t =
      raise (Failure "Not yet implemented")

    (* Improves a whole scc. *)
    let improve_scc (transitiongraph: TransitionGraph_.t)
                    (variablegraph: RVG.t)
                    (appr: Approximation_.t)
                    (scc: RVG.V.t list)
        : Approximation_.t  =
      match scc with
      | [rv] -> improve_trivial_scc transitiongraph appr rv
      | scc -> improve_nontrivial_scc transitiongraph variablegraph appr scc
      
    let improve transitiongraph appr =
      let module C = Graph.Components.Make(RVG) in
      let variablegraph = TransitionGraph_.create_variable_graph transitiongraph in
      List.fold_left (fun appr scc -> improve_scc transitiongraph variablegraph appr scc) appr (C.scc_list variablegraph)
      
  end
