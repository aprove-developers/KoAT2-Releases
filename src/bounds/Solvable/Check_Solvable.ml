open Batteries
open Polynomials

module Make (Bound : BoundType.Bound) (PM : ProgramTypes.ClassicalProgramModules) = struct
  open! PM
  module Loop = Loop.Make (Bound) (PM)

  type blocks = Var.t list list

  let check_solvable (t : Loop.t) =
    let module DG = Graph.Persistent.Digraph.ConcreteBidirectional (Var) in
    let module SCC = Graph.Components.Make (DG) in
    let dg_linear =
      Base.Set.fold
        ~f:(fun graph x ->
          let vars = Loop.update_var t x |> Polynomial.vars in
          Base.Set.fold ~f:(fun graph y -> DG.add_edge graph x y) vars ~init:graph)
        (Loop.updated_vars t) ~init:DG.empty
    and dg_non_linear =
      Base.Set.fold
        ~f:(fun graph x ->
          let update = Loop.update_var t x in
          let linear_vars =
            update |> Polynomial.vars
            |> Base.Set.filter ~f:(fun v -> not @@ Polynomial.var_only_linear v update)
          in
          Base.Set.fold ~f:(fun graph y -> DG.add_edge graph x y) linear_vars ~init:graph)
        (Loop.updated_vars t) ~init:DG.empty
    in
    let blocks = SCC.scc_list dg_linear in
    if
      List.for_all (fun scc -> List.length scc = 1) (SCC.scc_list dg_non_linear)
      (* We don't have cyclic non-linear dependencies. *)
      && List.for_all
           (fun block ->
             List.for_all
               (fun (x, y) -> Loop.update_var t x |> Polynomial.var_only_linear y)
               (List.cartesian_product block block))
           blocks
      (* For all blocks, all variable pairs (x,y) in such a block: The update of x only depends linearly on y. *)
    then
      Option.some blocks
    else
      None


  let check_solvable_ (_, t, _) = check_solvable @@ Loop.mk t
end
