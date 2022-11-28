open Batteries
open Polynomials
open ProgramModules

let check_solvable (t: TWNLoop.t) =
  let module DG = Graph.Persistent.Digraph.ConcreteBidirectional(Var) in
  let module SCC = Graph.Components.Make(DG) in
  let dg_linear = VarSet.fold (fun x graph ->
              let vars = TWNLoop.update t x |? Polynomial.of_var x |> Polynomial.vars in
              VarSet.fold (fun y graph -> DG.add_edge graph x y) vars graph) (TWNLoop.vars t) DG.empty and
  dg_non_linear = VarSet.fold (fun x graph ->
              let update = TWNLoop.update t x |? Polynomial.of_var x in
              let linear_vars = update
                |> Polynomial.vars
                |> VarSet.filter (fun v -> Polynomial.var_only_linear v update |> not) in
              VarSet.fold (fun y graph -> DG.add_edge graph x y) linear_vars graph) (TWNLoop.vars t) DG.empty in
  let blocks = SCC.scc_list dg_linear in
  if List.for_all (fun scc -> List.length scc = 1) (SCC.scc_list dg_non_linear) (* We don't have cyclic non-linear dependencies. *)
  && List.for_all (fun scc ->
                    List.for_all (fun x ->
                      List.for_all (fun y ->
                      TWNLoop.update t x
                      |? Polynomial.of_var x
                      |> Polynomial.var_only_linear y)
                      scc)
                    scc) blocks (* For all blocks, all variables x,y in such a block: The update of x only depends linear on y *) then
    Option.some blocks
  else
    None

let check_solvable_t (t: TransitionLabel.t) = check_solvable (TWNLoop.mk_transition t)
