open Batteries
open Constraints

module Location =
  struct
    type t = string [@@deriving eq, ord]

    let to_string l = l

    let hash l = Hashtbl.hash l

    let of_string name = name
  end

module LocationSet =
  struct
    include Set.Make(Location)

    let to_string =
      Util.enum_to_string Location.to_string % enum

    let powerset set =
      let combine (result: t Enum.t) (l: Location.t) = Enum.append result (Enum.map (fun ys -> add l ys) (Enum.clone result)) in
      Enum.fold combine (Enum.singleton empty) (enum set)
  end

module Transition =
  struct
    type t = Location.t * TransitionLabel.t * Location.t

    let equal equal_lbl (l1,t1,l1') (l2,t2,l2') =
      Location.equal l1 l2
      && equal_lbl t1 t2
      && Location.equal l1' l2'

    let same =
      equal TransitionLabel.same

    let same_gt =
      equal TransitionLabel.same_gt

    let equivalent =
      equal TransitionLabel.equivalent

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
      (l, TransitionLabel.add_invariant invariant t, l')

    let src (src, _, _) = src

    let label (_, label, _) = label

    let target (_, _, target) = target

    let id =
      TransitionLabel.id % label

    let cost t = TransitionLabel.cost (label t)

    let update_cost cvect (l,t,l') = (l,TransitionLabel.update_cost cvect t,l')
    let hash = Hashtbl.hash % id

    let to_id_string (l,label,l') =
      TransitionLabel.to_id_string label ^ ": " ^ Location.to_string l ^ "->" ^ Location.to_string l'

    let to_string ?(html=false) ~show_gtcost:bool (l,t,l') =
      let html_adapter =
        if html then
          let adapter = 
            fun c -> 
            match c with
              | '>' -> "&gt"
              | '<' -> "&lt;"
              | _  -> (String.make 1 c)
          in 
          (String.replace_chars adapter)
        else 
          identity
      in
          
      let probability =
        if (TransitionLabel.probability t) = (1. |> OurFloat.of_float) then
          ""
        else
          "p:"^(OurFloat.to_string (TransitionLabel.probability t))^":"
      in
      let cost =
        match (Polynomials.Polynomial.is_one (TransitionLabel.cost t)), (BoundsInst.RealBound.is_one (TransitionLabel.gtcost t)) with
        | (true , true) -> "->"
        | (false, true) -> "-{" ^ (Polynomials.Polynomial.to_string (TransitionLabel.cost t)) ^ ";}>"
        | (true, false) -> "-{;" ^ (BoundsInst.RealBound.show ~complexity:false (TransitionLabel.gtcost t)) ^ "}>"
        | (false, false) -> "-{" ^ (Polynomials.Polynomial.to_string (TransitionLabel.cost t)) ^ "; " ^ (BoundsInst.RealBound.show ~complexity:false (TransitionLabel.gtcost t)) ^ "}>"
      in
      (String.concat "" [(Location.to_string l); TransitionLabel.(update_to_string_lhs t); probability ; cost ; (Location.to_string l') ; TransitionLabel.(update_to_string_rhs t) ;" :|: " ;TransitionLabel.(guard_to_string t)])
      |> html_adapter

    let rename vars (l,t,l') =
      (l, (TransitionLabel.rename vars t),l')
  end

(*The equivalence test is needed in the probabilistic case, as we have transitions with branching degree >=2*)
module TransitionSet =
  struct
    include Set.Make(struct include Transition let compare = Transition.compare_same end)

    let powerset set =
      let combine (result: t Enum.t) (x: Transition.t) = Enum.append result (Enum.map (fun ys -> add x ys) (Enum.clone result)) in
      Enum.fold combine (Enum.singleton empty) (enum set)

    let to_string =
      Util.enum_to_string Transition.to_id_string % enum

    let total_probability tset =
      fold (fun t -> OurFloat.(+) (Transition.label t |> TransitionLabel.probability)) tset OurFloat.zero

  end

module GeneralTransition =
  struct
    type t = {
      id: int;
      transitions: TransitionSet.t;
      cost: BoundsInst.RealBound.t;
      start: Location.t;
      guard: TransitionLabel.Guard.t;
      guard_without_invariants: TransitionLabel.Guard.t;
      invariants: TransitionLabel.Guard.t;
    }

    let cost gt = gt.cost
    let transitions gtrans = gtrans.transitions
    let guard gtrans = gtrans.guard
    let guard_without_invariants gtrans = gtrans.guard_without_invariants
    let invariants gtrans = gtrans.invariants
    let id gtrans = gtrans.id
    let start gtrans = gtrans.start
    let compare gtrans1 gtrans2 = Int.compare gtrans1.id gtrans2.id
    let targets gtrans =
      gtrans
      |> transitions
      |> TransitionSet.elements
      |> List.map Transition.target
      |> LocationSet.of_list


    let same gt1 gt2 = gt1.id = gt2.id

    let of_transitionset transset (l,t,l') =
      let new_trans = TransitionSet.filter (fun (l2, t2, _) -> TransitionLabel.same_gt t t2 && Location.equal l l2) transset in
      {
        id = TransitionLabel.gt_id t; start = l; guard = TransitionLabel.guard t;
        cost = TransitionLabel.gtcost t; transitions = new_trans;
        guard_without_invariants = TransitionLabel.guard_without_invariants t; invariants = TransitionLabel.invariants t;
      }

    let to_string_helper (l,t,l') =
      let probability = if (TransitionLabel.probability t) = (1. |> OurFloat.of_float) then "" else (OurFloat.to_string (TransitionLabel.probability t))^":"
      in
      probability ^ (Location.to_string l') ^ TransitionLabel.(update_to_string_rhs t)

    let target_string gt =
      transitions gt
      |> TransitionSet.to_list
      |> List.map (fun t -> (Transition.label t |> TransitionLabel.probability, Transition.target t))
      |> List.map (fun (p,l) -> (OurFloat.to_string p) ^ ":" ^ (Location.to_string l))
      |> String.concat "; "
      |> fun str -> "[" ^ str ^ "]"

    let to_id_string gt =
      (Int.to_string % id) gt ^ ": " ^ (Location.to_string % start) gt ^ "->" ^ (target_string gt)

    let to_string gtrans =
      let any_label = gtrans |> transitions |> TransitionSet.any |> Transition.label in
      let trans_str = String.concat " :+: " (gtrans |> transitions |> TransitionSet.to_list |> List.map to_string_helper)
      and guard_str = if TransitionLabel.Guard.is_true (guard gtrans) then "" else ":|: " ^ (gtrans |> guard |> TransitionLabel.Guard.to_string)
      (*TODO Is this correct? every branch has same cost?*)
      and cost_str = if (Polynomials.Polynomial.is_one (TransitionLabel.cost any_label)) then "->" else "-{"^(Polynomials.Polynomial.to_string (TransitionLabel.cost any_label))^"}>"
      and start_str = (gtrans |> start |> Location.to_string) ^ (TransitionLabel.(update_to_string_lhs any_label))
      in
      String.concat " " [start_str; cost_str; trans_str; guard_str]

    let total_probability transition =
      TransitionSet.fold (fun trans rest -> Num.(+) (trans |> Transition.label |> TransitionLabel.probability) rest) (transitions transition)
        (0. |> OurFloat.of_float)

    let input_vars t =
      t.transitions
      |> TransitionSet.to_list
      |> List.map (VarSet.to_list % TransitionLabel.input_vars % Transition.label)
      |> List.flatten |> VarSet.of_list

    let vars t =
      t.transitions
      |> TransitionSet.to_list
      |> List.map (VarSet.to_list % TransitionLabel.vars % Transition.label)
      |> List.flatten |> VarSet.of_list

    let is_loop gtrans =
      gtrans
      |> targets
      |> LocationSet.elements
      |> List.for_all (fun loc -> Location.equal loc (start gtrans))
  end

module GeneralTransitionSet =
  struct
    include Set.Make(struct include GeneralTransition let compare = GeneralTransition.compare end)
    let to_string =
      Util.enum_to_string GeneralTransition.to_string % enum

    let to_id_string =
      Util.enum_to_string GeneralTransition.to_id_string % enum

    let start_locations transitions =
      fold (fun transition loc_set -> LocationSet.add (GeneralTransition.start transition) loc_set ) transitions LocationSet.empty

    let target_locations transitions =
      fold (fun transition loc_set -> LocationSet.union (GeneralTransition.targets transition) loc_set ) transitions LocationSet.empty

    let of_transitionset transitionset =
      TransitionSet.to_list transitionset
      |> List.map (GeneralTransition.of_transitionset transitionset)
      |> of_list
  end

(*The equivalence test is needed in the probabilistic case, as we have transitions with branching degree >=2*)
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

    let generalized_transitions graph =
      transitions graph |> GeneralTransitionSet.of_transitionset

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

    let to_string graph =
      let transition_str =
        transitions graph |> TransitionSet.enum |> Util.enum_to_string (Transition.to_id_string)
      in
      let location_str =
        locations graph |> LocationSet.enum |> Util.enum_to_string (Location.to_string)
      in
      "transitions: " ^ transition_str ^ " locations: " ^ location_str

  end

