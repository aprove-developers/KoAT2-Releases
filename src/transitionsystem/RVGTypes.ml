open! OurBase

module MakeRV
    (TL : ProgramTypes.TransitionLabel)
    (T : ProgramTypes.Transition with type transition_label = TL.t) =
struct
  type transition = T.t
  type transition_comparator_witness = T.comparator_witness
  type t = T.t * Var.t

  let hash (t, v) = Hashtbl.hash (T.id t, Var.to_string v)
  let transition (t, _) = t
  let variable (_, v) = v
  let to_id_string (t, v) = "|" ^ T.to_id_string t ^ "," ^ Var.to_string v ^ "|"

  let ids_to_string ?(pretty = false) (t, v) =
    TL.ids_to_string ~pretty (T.label t) ^ ", " ^ Var.to_string ~pretty v


  let sexp_of_t = Sexplib0.Sexp_conv.sexp_of_opaque

  type comparator_witness = (T.comparator_witness, Var.comparator_witness) RVComparator.comparator_witness

  let comparator = RVComparator.comparator T.comparator Var.comparator
  let compare = Comparator.compare_of_comparator comparator
  let equal = Comparator.equal_of_comparator comparator
end

module RV = MakeRV (TransitionLabel_) (Transition_)

module MakeRVG (PM : ProgramTypes.ClassicalProgramModules) = struct
  open PM
  module RV = MakeRV (TransitionLabel) (Transition)
  module G = Graph.Persistent.Digraph.ConcreteBidirectional (MakeRV (TransitionLabel) (Transition))
  module C = Graph.Components.Make (G)
  include G

  type scc = RV.t list

  let rvs_to_id_string rvs = rvs |> List.map ~f:RV.to_id_string |> String.concat ~sep:","
  let pre rvg rv = pred rvg rv
  let add_vertices_to_rvg vertices rvg = Sequence.fold ~f:add_vertex ~init:rvg vertices

  let rvg get_vars_in_lsb (program : Program.t) =
    let program_vars = Program.input_vars program in
    let add_transition (post_transition : Transition.t) (rvg : t) : t =
      let rvg_with_vertices : t =
        add_vertices_to_rvg
          (Set.to_sequence program_vars |> Sequence.map ~f:(fun var -> (post_transition, var)))
          rvg
      in
      (* Force evaluation of pre_transitions to avoid recomputation in pre_nodes *)
      let pre_transitions = Set.to_list @@ Program.pre program post_transition in
      let pre_nodes (post_var : Var.t) =
        get_vars_in_lsb (post_transition, post_var)
        |? VarSet.empty |> Set.to_sequence
        |> Sequence.cartesian_product (Sequence.of_list pre_transitions)
        |> Sequence.map ~f:(fun (pre_transition, pre_var) -> (pre_transition, pre_var, post_var))
      in
      program_vars |> Set.to_sequence |> Sequence.map ~f:pre_nodes |> Sequence.join
      |> Sequence.fold
           ~f:(fun rvg (pre_transition, pre_var, post_var) ->
             add_edge rvg (pre_transition, pre_var) (post_transition, post_var))
           ~init:rvg_with_vertices
    in
    TransitionGraph.fold_edges_e add_transition (Program.graph program) empty


  let rvg_with_sccs get_vars_in_lsb program =
    let rvg = rvg get_vars_in_lsb program in
    (rvg, Lazy.from_fun (fun () -> C.scc_list rvg))
end
