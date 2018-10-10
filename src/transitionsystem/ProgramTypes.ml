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

    let equal equal_lbl (l1,t1,l1') (l2,t2,l2') =
      Location.equal l1 l2
      && equal_lbl t1 t2
      && Location.equal l1' l2'
      
    let same =
      equal TransitionLabel.same
      
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
      (l, TransitionLabel.map_guard (Constraint.mk_and invariant) t, l')
      
    let src (src, _, _) = src
                        
    let label (_, label, _) = label

    let target (_, _, target) = target

    let id =
      TransitionLabel.id % label

    let cost t = TransitionLabel.cost (label t)

    let hash = Hashtbl.hash % id

    let to_id_string (l,label,l') =
      (Int.to_string % TransitionLabel.id) label ^ ": " ^ Location.to_string l ^ "->" ^ Location.to_string l'

    let to_string (l,t,l') =
      let probability = if (TransitionLabel.probability t) = 1. then "" else "p:"^(Float.to_string (TransitionLabel.probability t))^":" 
      and cost = if (Polynomials.Polynomial.is_one (TransitionLabel.cost t)) then "->" else "-{"^(Polynomials.Polynomial.to_string (TransitionLabel.cost t))^"}>" in
      String.concat "" [(Location.to_string l); TransitionLabel.(update_to_string_lhs t); probability ; cost ; (Location.to_string l') ; TransitionLabel.(update_to_string_rhs t) ;" :|: " ;TransitionLabel.(guard_to_string t)]

    let rename vars (l,t,l') =
      (l, (TransitionLabel.rename vars t),l')
  end

(*The equivalence test is needed in the probabilistic case, as we have transitions with branching degree >=2*)
module TransitionSet =
  struct
    include Set.Make(struct include Transition let compare = Transition.compare_equivalent end)

    let powerset set =
      let combine (result: t Enum.t) (x: Transition.t) = Enum.append result (Enum.map (fun ys -> add x ys) (Enum.clone result)) in
      Enum.fold combine (Enum.singleton empty) (enum set)

    let to_string =
      Util.enum_to_string Transition.to_id_string % enum
      
  end
  
(*The equivalence test is needed in the probabilistic case, as we have transitions with branching degree >=2*)
module TransitionGraph =
  struct
    include Graph.Persistent.Digraph.ConcreteBidirectionalLabeled(Location)(struct include TransitionLabel let compare = compare_equivalent end)

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

module GeneralTransition = 
  struct
    type t = {
      id: int;
      transitions: TransitionSet.t;
      start: Location.t;
      guard: TransitionLabel.Guard.t;
    }

    let transitions gtrans = gtrans.transitions
    let guard gtrans = gtrans.guard
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

    let from_transitionset transset (l,t,l') = 
      let new_trans = TransitionSet.filter (fun (l2, t2, l2') -> TransitionLabel.same t t2 && Location.equal l l2) transset in
      {
        id = TransitionLabel.id t; start = l; guard = TransitionLabel.guard t; transitions = new_trans
      }

    let to_string_helper (l,t,l') = 
      let probability = if (TransitionLabel.probability t) = 1. then "" else (Float.to_string (TransitionLabel.probability t))^":" 
      in
      probability ^ (Location.to_string l') ^ TransitionLabel.(update_to_string_rhs t)

    let target_string gt = 
      transitions gt
      |> TransitionSet.to_list
      |> List.map (fun t -> (Transition.label t |> TransitionLabel.probability, Transition.target t))
      |> List.map (fun (p,l) -> (Float.to_string p) ^ ":" ^ (Location.to_string l))
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
    TransitionSet.fold (fun trans rest -> (trans |> Transition.label |> TransitionLabel.probability) +. rest) (transitions transition) 0.

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

    let from_transitionset transitionset = 
      TransitionSet.to_list transitionset
      |> List.map (GeneralTransition.from_transitionset transitionset) 
      |> of_list 
  end
