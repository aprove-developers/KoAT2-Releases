(** Provides default modules to create locations, transitions and transitionsystems. *)
open Batteries
open Constraints

module Location =
  struct
    type t = string [@@deriving eq, ord]

    let to_string l = l

    let hash l = Hashtbl.hash l

    let of_string name = name
  end

module LocationSet = Set.Make(Location)

module Transition =
  struct
    type t = Location.t * TransitionLabel.t * Location.t




    let equal_ equal_lbl (l1,t1,l1') (l2,t2,l2') =
      Location.equal l1 l2
      && equal_lbl t1 t2
      && Location.equal l1' l2'

    let equal =
      equal_ TransitionLabel.same

    let same =
      equal_ TransitionLabel.same

    let equivalent =
      equal_ TransitionLabel.equivalent

    let compare compare_lbl (l1,t1,l1') (l2,t2,l2') =
      if Location.compare l1 l2 != 0 then
        Location.compare l1 l2
      else if compare_lbl t1 t2 != 0 then
        compare_lbl t1 t2
      else if Location.compare l1' l2' != 0 then
        Location.compare l1' l2'
      else
        0

    let compare_same =
      compare TransitionLabel.compare_same

    let compare_equivalent =
      compare TransitionLabel.compare_equivalent

    let add_invariant invariant (l,t,l') =
      (l, TransitionLabel.add_invariant t invariant, l')

    let src (src, _, _) = src

    let label (_, label, _) = label

    let target (_, _, target) = target

    let id =
      TransitionLabel.id % label

    (* let compare_same trans1 trans2 =
      Int.compare (id trans1) (id trans2)  *)

    let cost t = TransitionLabel.cost (label t)

    let hash = Hashtbl.hash % id

    let to_id_string (l,label,l') =
      (Int.to_string % TransitionLabel.id) label ^ ": " ^ Location.to_string l ^ "->" ^ Location.to_string l'

    let to_string ?(to_file = false) (l,t,l') =
      if to_file then
        (
          if (Constraint.is_true (TransitionLabel.guard t)) then
            ((Location.to_string l)
            ^ TransitionLabel.(update_to_string_lhs ~to_file t)
            ^ " -"^ (TransitionLabel.cost_to_string ~to_file t) ^ "> Com_1(" ^ (Location.to_string l')
            ^ TransitionLabel.(update_to_string_rhs ~to_file t) ^ ")")
          else
            (Location.to_string l)
            ^ TransitionLabel.(update_to_string_lhs ~to_file t)
            ^ " -"^ (TransitionLabel.cost_to_string ~to_file t) ^ "> Com_1(" ^ (Location.to_string l')
            ^ TransitionLabel.(update_to_string_rhs ~to_file t) ^ ") :|: "
            ^ TransitionLabel.(guard_to_string ~to_file t)
        )
      else
        Int.to_string (TransitionLabel.id t)^":"^Location.to_string l ^ TransitionLabel.(update_to_string_lhs t)^ " -"^
        TransitionLabel.cost_to_string t^"> " ^ Location.to_string l' ^
        TransitionLabel.update_to_string_rhs t ^ if Constraint.is_true (TransitionLabel.guard t) then "" else ":|:" ^ TransitionLabel.(guard_to_string t)

    let to_string_index (l,t,l') = 
      "t" ^ Util.natural_to_index (TransitionLabel.id t)^": "^Location.to_string l ^ TransitionLabel.(update_to_string_lhs_index t)^ " -"^
      TransitionLabel.cost_to_string t^"> " ^ Location.to_string l' ^
      TransitionLabel.update_to_string_rhs_index t ^ if Constraint.is_true (TransitionLabel.guard t) then "" else ":|:" ^ TransitionLabel.(guard_to_string t)

    let rename vars (l,t,l') =
      (l, (TransitionLabel.rename vars t),l')

    let rename2 rename_map (l,t,l') =
      (l, (TransitionLabel.rename2 rename_map t),l')

    let overapprox_nonlinear_updates (l,t,l') = l,TransitionLabel.overapprox_nonlinear_updates t,l'
  end

module TransitionSet =
  struct
    include Set.Make(struct include Transition let compare = Transition.compare_same end)

    let to_id_string = Util.enum_to_string Transition.to_id_string % enum

    let powerset set =
      let combine (result: t Enum.t) (x: Transition.t) = Enum.append result (Enum.map (fun ys -> add x ys) (Enum.clone result)) in
      Enum.fold combine (Enum.singleton empty) (enum set)

    let to_string =
      Util.enum_to_string Transition.to_id_string % enum

    let create f enum =
      enum
      |> Enum.map f
      |> of_enum

    let locations t =
      fold (fun (l,_,l') set -> LocationSet.add l set |> LocationSet.add l') t (LocationSet.empty)

  end



module TransitionGraph =
  struct
    include Graph.Persistent.Digraph.ConcreteBidirectionalLabeled(Location)(struct include TransitionLabel let compare = compare_same end)

    let locations graph =
      fold_vertex LocationSet.add graph LocationSet.empty

    let transitions graph =
      fold_edges_e TransitionSet.add graph TransitionSet.empty

    let loc_transitions graph locations =
      transitions graph
      |> TransitionSet.filter (fun (l,_,l') ->
             List.mem_cmp Location.compare l locations
             && List.mem_cmp Location.compare l' locations)

    module Equivalence_TransitionSet = Set.Make(struct include Transition let compare = Transition.compare_equivalent end)

    let equivalent graph1 graph2 =
      LocationSet.equal (locations graph1) (locations graph2)
      && Equivalence_TransitionSet.equal (graph1 |> transitions |> TransitionSet.enum |> Equivalence_TransitionSet.of_enum)
           (graph2 |> transitions |> TransitionSet.enum |> Equivalence_TransitionSet.of_enum)

    let replace_edge_e old_transition new_transition graph =
      add_edge_e (remove_edge_e graph old_transition) new_transition

    let add_invariant location invariant graph =
      location
      |> succ_e graph (* An invariant holds before the execution of the successor transitions *)
      |> List.fold_left (fun result transition ->
             replace_edge_e transition (Transition.add_invariant invariant transition) result
           ) graph

  end

module TransitionGraphWeight(Value : PolyTypes.Ring) =
  struct
    type t = Value.t
    type edge = TransitionGraph.E.t
    let weight (x : edge) = Value.one
    let compare x y = 0
    let add x y = Value.add x y
    let zero = Value.zero
  end
