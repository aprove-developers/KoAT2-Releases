open OurBase
open Polynomials

module Make (Bound : BoundType.Bound) (PM : ProgramTypes.ClassicalProgramModules) = struct
  open! PM
  module Loop = Loop.Make (Bound) (PM)

  (* TOPOLOGICAL ORDERING: *)
  let check_triangular (t : Loop.t) =
    let module DG = Graph.Persistent.Digraph.ConcreteBidirectional (Var) in
    let dependency_graph =
      Set.fold
        ~f:(fun graph x ->
          let vars = Polynomial.vars @@ Loop.update_var t x |> flip Set.add x in
          Set.fold ~f:(fun graph -> DG.add_edge graph x) vars ~init:graph)
        (Loop.updated_vars t) ~init:DG.empty
    in
    let module Topological = Graph.Topological.Make (DG) in
    let module SCC = Graph.Components.Make (DG) in
    if List.exists (SCC.scc_list dependency_graph) ~f:(( < ) 1 % List.length) then
      []
    else
      Topological.fold List.cons dependency_graph []


  (* MONOTONICITY *)
  let check_weakly_monotonicity (t : Loop.t) =
    Set.for_all
      ~f:(fun var ->
        let update = Loop.update_var t var in
        Polynomial.var_only_linear var update)
      (Loop.updated_vars t)


  (* NEGATIVITY *)
  let check_weakly_negativitiy (t : Loop.t) =
    Set.exists
      ~f:(fun var ->
        let update = Loop.update_var t var in
        update |> Polynomial.coeff_of_var var |> OurInt.is_negative)
      (Loop.updated_vars t)


  let check_twn loop =
    check_weakly_monotonicity loop
    && List.length (check_triangular loop) == Set.length (Loop.updated_vars loop)


  (* For Testing *)
  let check_twn_ (_, t, _) = check_twn (Loop.mk t)
end
