open Batteries
open Formulas
open Polynomials

module Make (PM : ProgramTypes.ClassicalProgramModules) = struct
  open PM
  module Loop = Loop.Make (PM)

  (* TOPOLOGICAL ORDERING: *)
  (* https://stackoverflow.com/questions/4653914/topological-sort-in-ocaml *)

  exception CycleFound of int list

  let dfs graph visited start_node =
    let rec explore path visited node =
      if List.mem node path then
        raise (CycleFound path)
      else if List.mem node visited then
        visited
      else
        let new_path = node :: path in
        let edges = List.assoc node graph in
        let visited = List.fold_left (explore new_path) visited edges in
        node :: visited
    in
    explore [] visited start_node


  let toposort graph = List.fold_left (fun visited (node, _) -> dfs graph visited node) [] graph

  let check_triangular (t : Loop.t) =
    let vars = Base.Set.to_list (Loop.updated_vars t) in
    let n = List.length vars in
    if n == 0 then
      []
    else
      (* We associate a natural number 0,..,|Vars|-1 to each variable.
         Then we build up the graph with an edge (n,m) if var (corresponding to) m occurs in the update of n.
         More precisely, we build a list xs with all such m's.
         The graph is a list consisting of tuples (n,xs) for all n. *)
      let vars_i = List.combine vars (List.range 0 `To (n - 1)) in
      let graph =
        List.mapi
          (fun i var ->
            let vars_update =
              Loop.update_var t var |> Polynomial.vars |> flip Base.Set.remove var |> Base.Set.to_list
              |> List.map (fun var -> List.assoc var vars_i)
            in
            (i, vars_update))
          vars
      in
      let order =
        try toposort graph with
        | CycleFound _ -> []
      in
      List.map (fun i -> List.assoc i (List.map Tuple2.swap vars_i)) order |> List.rev


  (* MONOTONICITY *)
  let check_weakly_monotonicity (t : Loop.t) =
    Base.Set.for_all
      ~f:(fun var ->
        let update = Loop.update_var t var in
        Polynomial.var_only_linear var update)
      (Loop.updated_vars t)


  (* NEGATIVITY *)
  let check_weakly_negativitiy (t : Loop.t) =
    Base.Set.exists
      ~f:(fun var ->
        let update = Loop.update_var t var in
        update |> Polynomial.coeff_of_var var |> OurInt.is_negative)
      (Loop.updated_vars t)


  let check_twn loop =
    check_weakly_monotonicity loop
    && List.length (check_triangular loop) == Base.Set.length (Loop.updated_vars loop)


  (* For Testing *)
  let check_twn_ (_, t, _) = check_twn (Loop.mk t)
end
