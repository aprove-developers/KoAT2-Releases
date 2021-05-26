open Batteries
open ProgramTypes

module RV =
  struct
    type t = Transition.t * Var.t

    let same (t1,v1) (t2,v2) =
      Transition.same t1 t2
      && Var.equal v1 v2

    let equivalent (t1,v1) (t2,v2) =
      Transition.equivalent t1 t2
      && Var.equal v1 v2

    let compare compare_transition (t1,v1) (t2,v2) =
      if compare_transition t1 t2 != 0 then
        compare_transition t1 t2
      else if Var.compare v1 v2 != 0 then
        Var.compare v1 v2
      else
        0

    let compare_same =
      compare Transition.compare_same

    let compare_equivalent =
      compare Transition.compare_equivalent

    let hash (t,v) =
      Hashtbl.hash (Transition.to_string t ^ Var.to_string v)

    let transition (t,_) = t

    let variable (_,v) = v

    let to_id_string (t,v) =
      "|" ^ Transition.to_id_string t ^ "," ^ Var.to_string v ^ "|"

  end

module RVG =
  struct
    include Graph.Persistent.Digraph.ConcreteBidirectional(struct
                include RV
                let equal = same
                let compare = compare_same
              end)

    type scc = RV.t list

    let rvs_to_id_string rvs =
      rvs
      |> List.map RV.to_id_string
      |> String.concat ","

    let pre rvg rv =
      pred rvg rv
      |> List.enum

    (* TODO Optimizable *)
    let entry_points rvg scc =
      scc
      |> List.enum
      |> Enum.map (pre rvg)
      |> Enum.flatten
      |> Enum.uniq_by RV.same
      |> Util.intersection RV.same (List.enum scc)

    let transitions scc =
      scc
      |> List.enum
      |> Enum.map RV.transition
      |> Enum.uniq_by Transition.same

    let add_vertices_to_rvg vertices rvg =
      vertices
      |> List.map (flip add_vertex)
      |> List.fold_left (fun rvg adder -> adder rvg) rvg

    let rvg kind (program: Program.t) =
      let add_transition (post_transition: Transition.t) (rvg: t): t =
        let rvg_with_vertices: t = add_vertices_to_rvg (program |> Program.vars |> VarSet.to_list |> List.map (fun var -> (post_transition,var))) rvg in
        let pre_nodes (post_transition: Transition.t) (post_var: Var.t) =
          LocalSizeBound.sizebound_local program kind post_transition post_var
          |> Option.map LocalSizeBound.vars
          |? Program.vars program
          |> VarSet.enum
          |> Enum.cartesian_product (Program.pre program post_transition)
          |> Enum.map (fun (pre_transition,pre_var) -> (pre_transition,pre_var,post_var))
        in
        Program.vars program
        |> VarSet.enum
        |> Enum.map (pre_nodes post_transition)
        |> Enum.flatten
        |> Enum.fold (fun rvg (pre_transition,pre_var,post_var) -> add_edge rvg (pre_transition,pre_var) (post_transition,post_var)) rvg_with_vertices
      in
      TransitionGraph.fold_edges_e add_transition (Program.graph program) empty

  end
