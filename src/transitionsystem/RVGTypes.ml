open Batteries
open ProgramTypes

module Make_RV (Trans :
                  sig
                    type t
                    val same: t -> t -> bool
                    val equivalent: t -> t -> bool
                    val compare_same: t -> t -> int
                    val compare_equivalent: t -> t -> int
                    val to_string: t -> string
                    val to_id_string: t -> string
                  end) =
  struct
    type t = Trans.t * Var.t

    let same (t1,v1) (t2,v2) =
      Trans.same t1 t2
      && Var.equal v1 v2

    let equivalent (t1,v1) (t2,v2) =
      Trans.equivalent t1 t2
      && Var.equal v1 v2

    let compare compare_transition (t1,v1) (t2,v2) =
      if compare_transition t1 t2 != 0 then
        compare_transition t1 t2
      else if Var.compare v1 v2 != 0 then
        Var.compare v1 v2
      else
        0

    let compare_same =
      compare Trans.compare_same

    let compare_equivalent =
      compare Trans.compare_equivalent

    let hash (t,v) =
      Hashtbl.hash (Trans.to_string t ^ Var.to_string v)

    let transition (t,_) = t

    let variable (_,v) = v

    let to_id_string (t,v) =
      "|" ^ Trans.to_id_string t ^ "," ^ Var.to_string v ^ "|"

  end

module RVG =
  struct
    module RV = Make_RV(struct
                          include Transition
                          let to_string = to_string ~show_gtcost:false
                        end)

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

    type kind = [`Lower | `Upper]
    type rvg_cache = (kind, t) Hashtbl.t
    let new_cache = fun () -> Hashtbl.create 2

    let rvg_ pre_cache lsb_cache kind (program: Program.t) =
      let add_transition (post_transition: Transition.t) (rvg: t): t =
        let pre_trans = Program.pre pre_cache program post_transition in

        let rvg_with_vertices: t = add_vertices_to_rvg (program |> Program.vars |> VarSet.to_list |> List.map (fun var -> (post_transition,var))) rvg in
        let pre_nodes (post_transition: Transition.t) (post_var: Var.t) =
          LocalSizeBound.sizebound_local lsb_cache program kind post_transition post_var
          |> Option.map LocalSizeBound.vars
          |? Program.vars program
          |> VarSet.to_list
          |> List.cartesian_product pre_trans
          |> List.map (fun (pre_transition,pre_var) -> (pre_transition,pre_var,post_var))
        in
        Program.vars program
        |> VarSet.to_list
        |> List.map (pre_nodes post_transition)
        |> List.flatten
        |> List.fold_left (fun rvg (pre_transition,pre_var,post_var) -> add_edge rvg (pre_transition,pre_var) (post_transition,post_var)) rvg_with_vertices
      in
      TransitionGraph.fold_edges_e add_transition (Program.graph program) empty

    let rvg rvg_cache pre_cache lsb_cache kind program =
      Util.memoize rvg_cache ~extractor:(fun (_,_,kind,_) -> kind)
        (fun (pre_cache, lsb_cache, kind, program) -> rvg_ pre_cache lsb_cache kind program)
      (pre_cache, lsb_cache, kind, program)

  end
