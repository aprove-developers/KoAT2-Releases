open OurBase
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
          let vars = Polynomial.vars @@ Loop.update_var t x |> flip Set.add x in
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
      List.for_all ~f:(fun scc -> List.length scc = 1) (SCC.scc_list dg_non_linear)
      (* We don't have cyclic non-linear dependencies. *)
      && List.for_all
           ~f:(fun block ->
             List.for_all
               ~f:(fun (x, y) -> Loop.update_var t x |> Polynomial.var_only_linear y)
               (List.cartesian_product block block))
           blocks
      (* For all blocks, all variable pairs (x,y) in such a block: The update of x only depends linearly on y. *)
    then
      (* Sort blocks w.r.t. twn-order. *)
      let map =
        Map.of_alist_exn (module Var) (List.map blocks ~f:(fun block -> (Var.fresh_id Int (), block)))
      in
      let dependency_graph =
        Map.fold map
          ~f:(fun ~key:x ~data:block1 graph ->
            Map.fold map
              ~f:(fun ~key:y ~data:block2 graph ->
                if
                  Var.equal x y
                  || not
                     @@ Set.are_disjoint (VarSet.of_list block1)
                          (VarSet.union_list @@ List.map ~f:(Polynomial.vars % Loop.update_var t) block2)
                then
                  DG.add_edge graph y x
                else
                  graph)
              ~init:graph)
          ~init:DG.empty
      in
      let module Topological = Graph.Topological.Make (DG) in
      Option.some @@ List.map (Topological.fold List.cons dependency_graph []) ~f:(Map.find_exn map)
    else
      None


  let check_solvable_ (_, t, _) = check_solvable @@ Loop.mk t

  let compute_update_matrix loop (block : Var.t list) =
    let open OurMatrix in
    (* get_linear_update_list (x<- 2x+3y+y^2) x [x;y] returns [2;3] *)
    let rec get_linear_update_of_var loop (block : Var.t list) (var_left : Var.t) =
      match block with
      | [] -> []
      | x :: xs ->
          Polynomial.coeff_of_indeterminate x (Loop.update_var loop var_left)
          :: get_linear_update_of_var loop xs var_left
    in
    List.map ~f:(get_linear_update_of_var loop block) block |> IntMatrix.of_list


  let compute_closed_form_ loop blocks =
    let substitute substitutions of_var x =
      List.find substitutions ~f:(Var.equal x % Tuple2.first) |? (x, of_var x) |> Tuple2.second
    in
    let theta, theta_inv, twn_update =
      List.map blocks ~f:(fun block ->
          let open OurMatrix in
          let update_matrix = compute_update_matrix loop block in
          let p_inv, j, p = IntMatrix.jordan_normal_form update_matrix in
          let theta = CAPolyMatrix.linear_map p block and theta_inv = CAPolyMatrix.linear_map p_inv block in
          Tuple3.make theta theta_inv
            (List.map theta
               ~f:
                 (Tuple2.map2
                 @@ CAPolynomial.substitute_f (CAPolynomial.of_intpoly % Map.find_exn (Loop.update loop)))
            |> List.map
                 ~f:(Tuple2.map2 @@ CAPolynomial.substitute_f (substitute theta_inv CAPolynomial.of_var))))
      |> List.unzip3
      |> Tuple3.map List.concat List.concat List.concat
    in
    let pe_twn =
      List.zip_exn (Tuple2.first @@ List.unzip twn_update)
      @@ PolyExponential.ComplexPE.compute_closed_form twn_update
    in
    let open PolyExponential in
    List.map theta_inv
      ~f:
        (Tuple2.map2
        @@ CAPolynomial.fold ~plus:ComplexPE.add ~const:ComplexPE.mk_cons ~times:ComplexPE.mul
             ~pow:ComplexPE.power ~indeterminate:(substitute pe_twn ComplexPE.mk_var))
    |> List.map ~f:(Tuple2.map2 @@ ComplexPE.substitute_f (substitute theta CAPolynomial.of_var))


  let compute_closed_form loop =
    let opt = check_solvable loop in
    if Option.is_some opt then
      Option.return @@ compute_closed_form_ loop (Option.value_exn opt)
    else
      None
end
