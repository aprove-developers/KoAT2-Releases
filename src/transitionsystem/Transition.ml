(** Default Transition over simple locations *)
include Batteries
include Constraints

(** Creates a Transition module for a given location type *)
module TransitionOver(TL: ProgramTypes.TransitionLabel)(L : ProgramTypes.Location) = struct
  type location = L.t
  type transition_label = TL.t
  type t = location * transition_label * location

  let equal_ equal_lbl (l1,t1,l1') (l2,t2,l2') =
    L.equal l1 l2
    && equal_lbl t1 t2
    && L.equal l1' l2'

  let equal =
    equal_ TL.same

  let same =
    equal_ TL.same

  let equivalent =
    equal_ TL.equivalent

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
    compare_f TL.compare_same

  let compare_equivalent =
    compare_f TL.compare_equivalent

  let compare = compare_same

  let add_invariant invariant (l,t,l') =
    (l, TL.add_invariant t invariant, l')

  let src (src, _, _) = src

  let label (_, label, _) = label

  let target (_, _, target) = target

  let id = TL.id % label

  let map_label f (l,label,l') = l, f label, l'

  let cost t = TL.cost (label t)

  let hash (transition: t) = (id transition) |> Hashtbl.hash

  let to_id_string (l,label,l') =
    (Int.to_string % TL.id) label ^ ": " ^ L.to_string l ^ "->" ^ L.to_string l'

  let to_id_string_pretty  (l,label,l') =
    "t" ^ Util.natural_to_subscript (TL.id label) ^ ": " ^ L.to_string l ^ "→" ^ L.to_string l'

  let to_string (l,t,l') =
    Int.to_string (TL.id t)^":"^L.to_string l ^ TL.(update_to_string_lhs t)^ " -"^
    TL.cost_to_string t^"> " ^ L.to_string l' ^
    TL.update_to_string_rhs t ^ if Constraint.is_true (TL.guard t) then "" else ":|:" ^ TL.(Guard.to_string (TL.guard t))

  let to_string_pretty (l,t,l') =
    "t" ^ Util.natural_to_subscript (TL.id t)^": "^L.to_string l ^ TL.(update_to_string_lhs_pretty t)^ (if Polynomials.Polynomial.(equal one (TL.cost t)) then " → " else " -"^ TL.cost_to_string t^"> " ) ^
    L.to_string l' ^
    TL.update_to_string_rhs_pretty t ^ if Constraint.is_true (TL.guard t) then "" else " :|: " ^ TL.(Guard.to_string ~pretty:true (TL.guard t))

  let rename vars (l,t,l') =
    (l, (TL.rename vars t),l')

  let overapprox_nonlinear_updates (l,t,l') = l,TL.overapprox_nonlinear_updates t,l'
end

module TransitionSetOver(T: ProgramTypes.Transition)(L: ProgramTypes.Location with type t = T.location) = struct
  include Set.Make(T)

  module LocationSet = Set.Make(L)

  type location_set = Set.Make(L).t

  let powerset set =
    let combine (result: t Enum.t) x = Enum.append result (Enum.map (fun ys -> add x ys) (Enum.clone result)) in
    Enum.fold combine (Enum.singleton empty) (enum set)

  let to_string =
    Util.enum_to_string T.to_id_string % enum

  let to_id_string = Util.enum_to_string T.to_id_string % enum

  let create f enum =
    of_enum (Enum.map f enum)

  let locations t =
    fold (fun (l,_,l') set -> LocationSet.add l set |> LocationSet.add l') t (LocationSet.empty)

  let targets = LocationSet.of_enum % Enum.map T.target % enum
end

include TransitionOver(TransitionLabel)(Location)

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
