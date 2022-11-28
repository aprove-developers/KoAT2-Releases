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

  let compare_f compare_lbl (l1,t1,l1') (l2,t2,l2') =
    if not (L.equal l1 l2) then
      L.compare l1 l2
    else if compare_lbl t1 t2 <> 0 then
      compare_lbl t1 t2
    else if not (L.equal l1' l2') then
      L.compare l1' l2'
    else
      0

  let compare_same =
    compare_f TransitionLabel.compare_same

  let compare_equivalent =
    compare_f TransitionLabel.compare_equivalent

  let compare = compare_same

  let add_invariant invariant (l,t,l') =
    (l, TransitionLabel.add_invariant t invariant, l')

  let src (src, _, _) = src

  let label (_, label, _) = label

  let target (_, _, target) = target

  let id = TransitionLabel.id % label

  let map_label f (l,label,l') = l, f label, l'

  (* let compare_same trans1 trans2 =
    Int.compare (id trans1) (id trans2)  *)

  let cost t = TransitionLabel.cost (label t)

  let hash (transition: t) = (id transition) |> Hashtbl.hash

  let to_id_string (l,label,l') =
    (Int.to_string % TransitionLabel.id) label ^ ": " ^ L.to_string l ^ "->" ^ L.to_string l'

  let to_id_string_pretty  (l,label,l') =
    "t" ^ Util.natural_to_subscript (TransitionLabel.id label) ^ ": " ^ L.to_string l ^ "→" ^ L.to_string l'

  let to_string (l,t,l') =
    Int.to_string (TransitionLabel.id t)^":"^L.to_string l ^ TransitionLabel.(update_to_string_lhs t)^ " -"^
    TransitionLabel.cost_to_string t^"> " ^ L.to_string l' ^
    TransitionLabel.update_to_string_rhs t ^ if Constraint.is_true (TransitionLabel.guard t) then "" else ":|:" ^ TransitionLabel.(Guard.to_string (TransitionLabel.guard t))

  let to_string_pretty (l,t,l') =
    "t" ^ Util.natural_to_subscript (TransitionLabel.id t)^": "^L.to_string l ^ TransitionLabel.(update_to_string_lhs_pretty t)^
    (if Polynomials.Polynomial.(equal one (TransitionLabel.cost t)) then " → " else " -"^ TransitionLabel.cost_to_string t^"> " ) ^
    L.to_string l' ^
    TransitionLabel.update_to_string_rhs_pretty t ^ if Constraint.is_true (TransitionLabel.guard t) then "" else " :|: " ^ TransitionLabel.(Guard.to_string ~pretty:true (TransitionLabel.guard t))

  let rename vars (l,t,l') =
    (l, (TransitionLabel.rename vars t),l')

  let overapprox_nonlinear_updates (l,t,l') = l,TransitionLabel.overapprox_nonlinear_updates t,l'
end

module TransitionSetOver(L: ProgramTypes.Location) = struct
  include Set.Make(TransitionOver(L))

  module Transition = TransitionOver(L)
  module LocationSet = Set.Make(L)

  type location_set = Set.Make(L).t

  let powerset set =
    let combine (result: t Enum.t) x = Enum.append result (Enum.map (fun ys -> add x ys) (Enum.clone result)) in
    Enum.fold combine (Enum.singleton empty) (enum set)

  let to_string =
    Util.enum_to_string Transition.to_id_string % enum

  let to_id_string = Util.enum_to_string Transition.to_id_string % enum

  let create f enum =
    of_enum (Enum.map f enum)

  let locations t =
    fold (fun (l,_,l') set -> LocationSet.add l set |> LocationSet.add l') t (LocationSet.empty)

  let targets = LocationSet.of_enum % Enum.map Transition.target % enum
end

include TransitionOver(Location)

let to_file_string (l,t,l') =
  let without_guard =
    Location.to_string l
    ^ TransitionLabel.update_to_file_string_lhs t
    ^ " -" ^ TransitionLabel.cost_to_string t ^ "> Com_1(" ^ Location.to_string l'
    ^ TransitionLabel.update_to_file_string_rhs t ^ ")"
  in
  if Constraint.is_true (TransitionLabel.guard t) then
    without_guard ^ " :|: " ^ Guard.to_file_string (TransitionLabel.guard t)
  else
    without_guard
