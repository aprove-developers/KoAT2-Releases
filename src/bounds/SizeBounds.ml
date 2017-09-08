open Batteries

module Make(A : BoundTypes.Approximation) =
  struct

    module Approximation_ = A

    type classification = Equality | Constant | ScaledSum | Unbound

    let classify (poly: A.bound): classification =
      raise (Failure "Not yet implemented")

    let improve_scc (graph: Approximation_.TransitionGraph_.VariableGraph_.Graph.t)
                    (appr: Approximation_.t)
                    (scc: Approximation_.TransitionGraph_.VariableGraph_.Graph.V.t list): A.t  =
      match scc with
      | [result_variable] -> raise (Failure "Not yet implemented")
      | result_variables -> raise (Failure "Not yet implemented")
      
    let improve graph appr =
      let module C = Graph.Components.Make(Approximation_.TransitionGraph_.VariableGraph_.Graph) in
      List.fold_left (fun appr scc -> improve_scc graph appr scc) appr (C.scc_list graph)
      
  end
