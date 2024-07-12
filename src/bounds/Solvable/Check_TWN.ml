open OurBase
open Polynomials

module Make (Bound : BoundType.Bound) (PM : ProgramTypes.ClassicalProgramModules) = struct
  open! PM
  module Loop = Loop.Make (Bound) (PM)

  type twn = TWN of Var.t list | NOTTRIANGULAR

  let eval_twn = function
    | TWN _ -> true
    | _ -> false


  exception NOT_TWN

  let unwrap_twn = function
    | TWN xs -> xs
    | NOTTRIANGULAR -> raise NOT_TWN


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
      NOTTRIANGULAR
    else
      TWN (Topological.fold List.cons dependency_graph [])


  type monotonicity = MONOTONIC | DEFECTIVE of Var.t list

  let eval_monotonicity = function
    | MONOTONIC -> true
    | _ -> false


  let defective_vars = function
    | MONOTONIC -> []
    | DEFECTIVE xs -> xs


  (* MONOTONICITY *)
  let check_weakly_monotonicity (t : Loop.t) =
    let defective =
      Set.filter
        ~f:(fun var ->
          let update = Loop.update_var t var in
          not @@ Polynomial.var_only_linear var update)
        (Loop.updated_vars t)
    in
    if Set.is_empty defective then
      MONOTONIC
    else
      (* Compute defective vars. *)
      let module DG = Graph.Persistent.Digraph.ConcreteBidirectional (Var) in
      let dependency_graph =
        Set.fold
          ~f:(fun graph x ->
            let vars = Polynomial.vars @@ Loop.update_var t x in
            Set.fold ~f:(fun graph y -> DG.add_edge graph y x) vars ~init:graph)
          (Loop.updated_vars t) ~init:DG.empty
      in
      let module Path = Graph.Path.Check (DG) in
      let path_checker = Path.create dependency_graph in
      DEFECTIVE
        (Set.to_list
           (Set.fold defective ~init:defective ~f:(fun set v ->
                let reachable_by_v = Set.filter (Loop.vars t) ~f:(Path.check_path path_checker v) in
                Set.union set reachable_by_v)))


  (* NEGATIVITY *)
  let check_weakly_negativitiy (t : Loop.t) =
    Set.exists
      ~f:(fun var ->
        let update = Loop.update_var t var in
        update |> Polynomial.coeff_of_var var |> OurInt.is_negative)
      (Loop.updated_vars t)


  let check_twn loop =
    (eval_monotonicity @@ check_weakly_monotonicity loop) && (eval_twn @@ check_triangular loop)
end
