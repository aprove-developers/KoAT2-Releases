open OurBase
open Polynomials

module Make (Bound : BoundType.Bound) (PM : ProgramTypes.ClassicalProgramModules) = struct
  open! PM
  module Loop = Loop.Make (Bound) (PM)

  type t = TWN of Var.t list | NOTTWN of Var.t list

  let eval_twn = function
    | TWN _ -> true
    | _ -> false


  let unwrap_twn = function
    | TWN xs -> xs
    | NOTTWN xs -> xs


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
      NOTTWN (List.concat @@ List.filter (SCC.scc_list dependency_graph) ~f:(( < ) 1 % List.length))
    else
      TWN (Topological.fold List.cons dependency_graph [])


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


  let check_twn loop = check_weakly_monotonicity loop && (eval_twn @@ check_triangular loop)

  (* For Testing *)
  let check_twn_t (_, t, _) = check_twn (Loop.mk t)
end
