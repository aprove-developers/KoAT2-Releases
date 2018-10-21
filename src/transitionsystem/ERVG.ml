open Batteries
open ProgramTypes
open RVGTypes

module RV = Make_RV(RVTransitions.TransitionForExpectedSize)

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
  |> Enum.uniq_by RVTransitions.TransitionForExpectedSize.same

let add_vertices_to_rvg vertices rvg =
  vertices
  |> List.map (flip add_vertex)
  |> List.fold_left (fun rvg adder -> adder rvg) rvg

let rvg kind (program: Program.t) =
  let add_gt (post_gt: GeneralTransition.t) (rvg: t): t =
    let rvg_with_vertices: t =
      add_vertices_to_rvg
        (program |> Program.vars |> VarSet.to_list |> List.map (fun var -> (post_gt,var))
         |> List.cartesian_product (GeneralTransition.targets post_gt |> LocationSet.to_list)
         |> List.map (fun (l,(gt,v)) -> ((gt,l),v) ) ) rvg
    in
    let pre_nodes (post_transition: GeneralTransition.t) (l: Location.t) (post_var: Var.t) =
      ExpLocalSizeBound.vars ((post_transition,l),post_var)
      |> VarSet.enum
      |> Enum.cartesian_product (Program.pre_gt program post_transition
                                 |> GeneralTransitionSet.enum
                                 |> Enum.map (fun gt' -> (gt',GeneralTransition.start post_transition)))
      |> failwith ""
      |> Enum.map (fun (gt,pre_transition,pre_var) -> ((gt,pre_transition),pre_var,post_var))
      |> failwith ""
    in
    Program.vars program
    |> VarSet.enum
    |> Enum.cartesian_product (GeneralTransition.targets post_gt |> LocationSet.enum)
    |> Enum.map (fun (l,v) -> pre_nodes post_gt l v)
    |> Enum.flatten
    |> Enum.fold (fun rvg ((pre_gt,l),pre_var,post_var) -> add_edge rvg ((pre_gt,l),pre_var) ((pre_gt,l),post_var)) rvg_with_vertices
  in
  Program.transitions program
  |> GeneralTransitionSet.from_transitionset
  |> fun gtset -> GeneralTransitionSet.fold add_gt gtset empty
