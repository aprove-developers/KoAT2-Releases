(** Default Transition over simple locations *)
include Batteries
include Constraints

(** Creates a Transition module for a given location type *)
module TransitionOver(L : ProgramTypes.Location) = struct
  type location = L.t
  type t = location * TransitionLabel.t * location

  let equal_ equal_lbl (l1,t1,l1') (l2,t2,l2') =
    L.equal l1 l2
    && equal_lbl t1 t2
    && L.equal l1' l2'

  let equal =
    equal_ TransitionLabel.same

  let same =
    equal_ TransitionLabel.same

  let equivalent =
    equal_ TransitionLabel.equivalent

  let compare compare_lbl (l1,t1,l1') (l2,t2,l2') =
    if L.compare l1 l2 != 0 then
      L.compare l1 l2
    else if compare_lbl t1 t2 != 0 then
      compare_lbl t1 t2
    else if L.compare l1' l2' != 0 then
      L.compare l1' l2'
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

  let id = TransitionLabel.id % label

  (* let compare_same trans1 trans2 =
    Int.compare (id trans1) (id trans2)  *)

  let cost t = TransitionLabel.cost (label t)

  let hash (transition: t) = (id transition) |> Hashtbl.hash

  let to_id_string (l,label,l') =
    (Int.to_string % TransitionLabel.id) label ^ ": " ^ L.to_string l ^ "->" ^ L.to_string l'

  let to_id_string_pretty  (l,label,l') =
    "t" ^ Util.natural_to_subscript (TransitionLabel.id label) ^ ": " ^ L.to_string l ^ "→" ^ L.to_string l'

  let to_string ?(to_file = false) (l,t,l') =
    if to_file then
      (
        if (Constraint.is_true (TransitionLabel.guard t)) then
          ((L.to_string l)
          ^ TransitionLabel.(update_to_string_lhs ~to_file t)
          ^ " -"^ (TransitionLabel.cost_to_string ~to_file t) ^ "> Com_1(" ^ (L.to_string l')
          ^ TransitionLabel.(update_to_string_rhs ~to_file t) ^ ")")
        else
          (L.to_string l)
          ^ TransitionLabel.(update_to_string_lhs ~to_file t)
          ^ " -"^ (TransitionLabel.cost_to_string ~to_file t) ^ "> Com_1(" ^ (L.to_string l')
          ^ TransitionLabel.(update_to_string_rhs ~to_file t) ^ ") :|: "
          ^ TransitionLabel.(guard_to_string ~to_file t)
      )
    else
      Int.to_string (TransitionLabel.id t)^":"^L.to_string l ^ TransitionLabel.(update_to_string_lhs t)^ " -"^
      TransitionLabel.cost_to_string t^"> " ^ L.to_string l' ^
      TransitionLabel.update_to_string_rhs t ^ if Constraint.is_true (TransitionLabel.guard t) then "" else ":|:" ^ TransitionLabel.(guard_to_string t)

  let to_string_pretty (l,t,l') =
    "t" ^ Util.natural_to_subscript (TransitionLabel.id t)^": "^L.to_string l ^ TransitionLabel.(update_to_string_lhs_pretty t)^
    (if Polynomials.Polynomial.(equal one (TransitionLabel.cost t)) then " → " else " -"^ TransitionLabel.cost_to_string t^"> " ) ^
    L.to_string l' ^
    TransitionLabel.update_to_string_rhs_pretty t ^ if Constraint.is_true (TransitionLabel.guard t) then "" else " :|: " ^ TransitionLabel.(guard_to_string ~pretty:true t)

  let rename vars (l,t,l') =
    (l, (TransitionLabel.rename vars t),l')

  let overapprox_nonlinear_updates (l,t,l') = l,TransitionLabel.overapprox_nonlinear_updates t,l'
end

include TransitionOver(Location)
