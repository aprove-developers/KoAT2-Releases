open Batteries

module MakeRV(TL: ProgramTypes.TransitionLabel)
             (T: ProgramTypes.Transition with type transition_label = TL.t) =
  struct
    let compare compare_transition (t1,v1) (t2,v2) =
      if compare_transition t1 t2 != 0 then
        compare_transition t1 t2
      else if Var.compare v1 v2 != 0 then
        Var.compare v1 v2
      else
        0

    module RVTuple_ = struct
      type transition = T.t
      type t = T.t * Var.t
      let equal (t1,v1) (t2,v2)= T.same t1 t2 && Var.equal v1 v2
      let hash (t,v) = Hashtbl.hash (T.id t, Var.to_string v)
      let compare = compare T.compare_same
    end

    type transition = RVTuple_.transition
    type t = RVTuple_.t

    let same = RVTuple_.equal

    let equivalent (t1,v1) (t2,v2) =
      T.equivalent t1 t2
      && Var.equal v1 v2

    let hash = RVTuple_.hash

    let compare_same =
      compare T.compare_same

    let compare_equivalent =
      compare T.compare_equivalent

    let transition (t,_) = t

    let variable (_,v) = v

    let to_id_string (t,v) =
      "|" ^ T.to_id_string t ^ "," ^ Var.to_string v ^ "|"

    let ids_to_string ?(pretty=false) (t,v) =
      TL.ids_to_string ~pretty (T.label t) ^ ", " ^ Var.to_string ~pretty v

  end

module RV = MakeRV(TransitionLabel_)(Transition_)

module MakeRVG(PM: ProgramTypes.ClassicalProgramModules) =
  struct
    open PM

    module RV = MakeRV(TransitionLabel)(Transition)

    module G = Graph.Persistent.Digraph.ConcreteBidirectional(struct
                include MakeRV(TransitionLabel)(Transition)
                let equal = same
                let compare = compare_same
              end)
    module C = Graph.Components.Make(G)
    include G

    type scc = RV.t list

    let rvs_to_id_string rvs =
      rvs
      |> List.map RV.to_id_string
      |> String.concat ","

    let pre rvg rv =
      pred rvg rv
      |> List.enum

    let add_vertices_to_rvg vertices rvg =
      Enum.fold add_vertex rvg vertices

    let rvg get_vars_in_lsb (program: Program.t) =
      let program_vars = Program.input_vars program in
      let add_transition (post_transition: Transition.t) (rvg: t): t =
        let rvg_with_vertices: t =
          add_vertices_to_rvg
            (VarSet.enum program_vars |> Enum.map (fun var -> (post_transition,var)))
            rvg
        in
        (* Force evaluation of pre_transitions to avoid recomputation in pre_nodes *)
        let pre_transitions = TransitionSet.to_list @@ Program.pre_transitionset_cached program post_transition in
        let pre_nodes (post_var: Var.t) =
          get_vars_in_lsb (post_transition,post_var)
          |? VarSet.empty
          |> VarSet.enum
          |> Enum.cartesian_product (List.enum pre_transitions)
          |> Enum.map (fun (pre_transition,pre_var) -> (pre_transition,pre_var,post_var))
        in
        program_vars
        |> VarSet.enum
        |> Enum.map pre_nodes
        |> Enum.flatten
        |> Enum.fold (fun rvg (pre_transition,pre_var,post_var) -> add_edge rvg (pre_transition,pre_var) (post_transition,post_var)) rvg_with_vertices
      in
      TransitionGraph.fold_edges_e add_transition (Program.graph program) empty

    let rvg_with_sccs get_vars_in_lsb program =
      let rvg = rvg get_vars_in_lsb program in
      rvg, Lazy.from_fun (fun () -> C.scc_list rvg)

  end
