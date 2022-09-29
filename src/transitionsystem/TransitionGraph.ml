open Batteries

module TransitionGraphOver(L : ProgramTypes.Location) = struct
  module Location = L
  module LocationSet = Set.Make(Location)
  module Transition = Transition.TransitionOver(Location)
  module TransitionSet = struct
    include Set.Make(struct include Transition let compare = Transition.compare_same end)
    type locationSet = LocationSet.t
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

  include Graph.Persistent.Digraph.ConcreteBidirectionalLabeled(Location)(struct include TransitionLabel let compare = compare_same end)

  let add_locations locations graph = Enum.fold add_vertex graph locations

  let add_transitions transitions graph = Enum.fold add_edge_e graph transitions

  let mk transitions = empty |> add_transitions transitions

  let locations graph = fold_vertex LocationSet.add graph LocationSet.empty

  let transitions graph = fold_edges_e TransitionSet.add graph TransitionSet.empty

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

include TransitionGraphOver(Location)