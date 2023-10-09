include OurBase
(** Default Transition over simple locations *)

include Constraints

(** Creates a Transition module for a given location type *)
module Make (TL : ProgramTypes.TransitionLabel) = struct
  type transition_label = TL.t
  type transition_label_comparator_witness = TL.comparator_witness
  type t = Location.t * transition_label * Location.t
  type comparator_witness = TL.comparator_witness TransitionComparator.comparator_witness

  let comparator = TransitionComparator.comparator TL.comparator
  let sexp_of_t = Sexplib0.Sexp_conv.sexp_of_opaque
  let equal = Comparator.equal_of_comparator comparator

  let equivalent (l1, t1, l1') (l2, t2, l2') =
    Location.equal l1 l2 && TL.equivalent t1 t2 && Location.equal l1' l2'


  let compare = Comparator.compare_of_comparator comparator

  let compare_equivalent (l1, t1, l1') (l2, t2, l2') =
    if not (Location.equal l1 l2) then
      Location.compare l1 l2
    else if TL.compare_equivalent t1 t2 <> 0 then
      TL.compare_equivalent t1 t2
    else if not (Location.equal l1' l2') then
      Location.compare l1' l2'
    else
      0


  let src (src, _, _) = src
  let label (_, label, _) = label
  let target (_, _, target) = target
  let id = TL.id % label
  let map_label f (l, label, l') = (l, f label, l')
  let cost t = TL.cost (label t)
  let hash (transition : t) = id transition |> Hashtbl.hash

  let to_id_string (l, label, l') =
    (Int.to_string % TL.id) label ^ ": " ^ Location.to_string l ^ "->" ^ Location.to_string l'


  let to_id_string_pretty (l, label, l') =
    "t" ^ Util.natural_to_subscript (TL.id label) ^ ": " ^ Location.to_string l ^ "→" ^ Location.to_string l'


  let to_string (l, t, l') =
    Int.to_string (TL.id t)
    ^ ":" ^ Location.to_string l
    ^ TL.(update_to_string_lhs t)
    ^ " -" ^ TL.cost_to_string t ^ "> " ^ Location.to_string l' ^ TL.update_to_string_rhs t
    ^
    if Constraint.is_true (TL.guard t) then
      ""
    else
      ":|:" ^ Guard.to_string (TL.guard t)


  let to_string_pretty (l, t, l') =
    "t"
    ^ Util.natural_to_subscript (TL.id t)
    ^ ": " ^ Location.to_string l
    ^ TL.(update_to_string_lhs_pretty t)
    ^ (if Polynomials.Polynomial.(equal one (TL.cost t)) then
         " → "
       else
         " -" ^ TL.cost_to_string t ^ "> ")
    ^ Location.to_string l' ^ TL.update_to_string_rhs_pretty t
    ^
    if Constraint.is_true (TL.guard t) then
      ""
    else
      " :|: " ^ Guard.to_string ~pretty:true (TL.guard t)
end

module MakeClassical (TL : ProgramTypes.ClassicalTransitionLabel) = struct
  include Make (TL)

  let overapprox_nonlinear_updates (l, t, l') = (l, TL.overapprox_nonlinear_updates t, l')
end

open! OurBase

module TransitionSetOver (T : ProgramTypes.Transition) = struct
  include Set
  include MakeSetCreators0 (T)

  let to_string s = Util.sequence_to_string ~f:T.to_id_string (Set.to_sequence s)
  let to_id_string s = Util.sequence_to_string ~f:T.to_id_string (Set.to_sequence s)
  let to_id_string_pretty s = Util.sequence_to_string ~f:T.to_id_string_pretty (Set.to_sequence s)

  let locations : t -> LocationSet.t =
    Set.fold ~f:(fun set (l, _, l') -> Set.add (Set.add set l) l') ~init:LocationSet.empty


  let targets : t -> LocationSet.t = LocationSet.map ~f:T.target

  let find_by_id set id =
    Set.binary_search set ~compare:(fun t i -> Int.compare (T.id t) i) `First_equal_to id


  let find_by_ids set ids =
    Sequence.map ~f:(find_by_id set) ids |> Sequence.filter_opt |> Set.of_sequence (module T)
end

include MakeClassical (TransitionLabel_)

let to_file_string (l, t, l') =
  let without_guard =
    Location.to_string l
    ^ TransitionLabel_.update_to_file_string_lhs t
    ^ " -" ^ TransitionLabel_.cost_to_string t ^ "> Com_1(" ^ Location.to_string l'
    ^ TransitionLabel_.update_to_file_string_rhs t
    ^ ")"
  in
  if Constraint.is_true (TransitionLabel_.guard t) then
    without_guard
  else
    without_guard ^ " :|: " ^ Guard.to_file_string (TransitionLabel_.guard t)


let rename vars (l, t, l') = (l, TransitionLabel_.rename vars t, l')
