open Batteries
open Polynomials

(* TOPOLOGICAL ORDERING: *)

(* https://stackoverflow.com/questions/4653914/topological-sort-in-ocaml *)
exception CycleFound of int list

module Make(PM: ProgramTypes.ClassicalProgramModules) = struct
  open PM

  module TWNLoop = TWNLoop.Make(PM)

  let dfs graph visited start_node =
    let rec explore path visited node =
      if List.mem node path    then raise (CycleFound path) else
      if List.mem node visited then visited else
        let new_path = node :: path in
        let edges    = List.assoc node graph in
        let visited  = List.fold_left (explore new_path) visited edges in
        node :: visited
    in explore [] visited start_node

  let toposort graph =
    List.fold_left (fun visited (node,_) -> dfs graph visited node) [] graph

  let check_triangular (t: TWNLoop.t) =
    let vars = VarSet.to_list (TWNLoop.input_vars t) in
    let n = List.length vars in
    if (n == 0) then []
    else
      let vars_i = List.combine vars (List.range 0 `To (n - 1)) in
      let graph = List.mapi (fun i var ->
        let vars_update =
            TWNLoop.update t var
            |? Polynomial.zero
            |> Polynomial.vars
            |> VarSet.remove var
            |> VarSet.to_list
            |> List.map (fun var -> List.assoc var vars_i) in (i, vars_update)) vars in
      let order = try toposort graph with CycleFound _ -> [] in
      List.map (fun i -> List.assoc i (List.map Tuple2.swap vars_i)) order
      |> List.rev

  let check_triangular_t (t: TransitionLabel.t) = check_triangular (TWNLoop.mk_transition t)

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
      Option.some (blocks)
    else
      None

  let check_solvable_t (t: TransitionLabel.t) = check_solvable (TWNLoop.mk_transition t)

  (* MONOTONICITY *)
  let check_weakly_monotonicity (t: TWNLoop.t) =
    VarSet.for_all (fun var -> let update = TWNLoop.update t var in
                            if Option.is_none update then
                              true
                            else
                              update |> Option.get |> Polynomial.var_only_linear var) (TWNLoop.input_vars t)


  (* NEGATIVITY *)
  let check_weakly_negativitiy (t: TWNLoop.t) =
    VarSet.exists (fun var -> let update = TWNLoop.update t var in
                            if Option.is_none update then
                              false
                            else
                              update |> Option.get |> Polynomial.coeff_of_var var |> OurInt.is_negative) (TWNLoop.input_vars t)

  let chain (t: TWNLoop.t) = TWNLoop.append t t

  (* For Testing *)
  let check_twn loop =
    (check_weakly_monotonicity loop) && ((List.length (check_triangular loop)) == (VarSet.cardinal ((TWNLoop.input_vars loop))))

  let check_twn (_,t,_) =
   check_twn (TWNLoop.mk_transition t)
end
