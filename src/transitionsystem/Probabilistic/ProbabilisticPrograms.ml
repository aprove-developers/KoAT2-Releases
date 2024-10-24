open! OurBase
open Formulas
open Polynomials

exception ProbabilitiesDoNotSumToOne

module Invariant = Guard

type label_without_backlink = {
  probability : RationalLaurentPolynomial.t;
  overappr_guard : Guard.t;
  update : UpdateElement_.t ProgramTypes.VarMap.t;
  overappr_nonprob_update : Polynomials.Polynomial.t ProgramTypes.VarMap.t;
      (** Non-determinstically Overapproximated non-probabilistic versions for classic analysis *)
}

type 'a ptr = 'a Option.t ref

type 'trans_label_cmp transition_label_ = {
  id : int;
  gt : 'trans_label_cmp general_transition_ ptr;
  properties : label_without_backlink;
}

and 'trans_label_cmp transition_ = Location.t * 'trans_label_cmp transition_label_ * Location.t
and 'trans_label_cmp trans_comparator_ = 'trans_label_cmp TransitionComparator.comparator_witness

and 'trans_label_cmp general_transition_ = {
  gt_id : int;
  guard : Guard.t;
  invariant : Invariant.t;
  cost : Polynomial.t;
  transitions : ('trans_label_cmp transition_, 'trans_label_cmp trans_comparator_) Set.t;
}

module LabelComparator = Comparator.Make1 (struct
  type 'trans_label_cmp t = 'trans_label_cmp transition_label_

  let sexp_of_t = Sexplib0.Sexp_conv.sexp_of_opaque
  let compare label1 label2 = Int.compare label1.id label2.id
end)

type transition_label = LabelComparator.comparator_witness transition_label_
type general_transition = LabelComparator.comparator_witness general_transition_

(** This is a low level internal interface *)
module ProbabilisticTransitionLabel_ = struct
  type t = transition_label

  module Invariant = Guard

  let sexp_of_t = Sexplib0.Sexp_conv.sexp_of_opaque

  let default_label_without_backlink =
    {
      probability = RationalLaurentPolynomial.one;
      overappr_guard = Guard.mk_true;
      (* This guard goes along with overappr_nonprob_update *)
      update = Map.empty (module Var);
      overappr_nonprob_update = Map.empty (module Var);
    }


  let default =
    {
      id = 0;
      (* We omit the general transition here, as the default label should not be used anywhere.
         Otherwise one would have to provide a start and target location for the transition corresponding
         to this label which are unknown here *)
      gt = ref None;
      properties = default_label_without_backlink;
    }


  let compute_overapproximated_update_and_guard update_map =
    Map.map
      ~f:(fun ue ->
        match UpdateElement_.to_polynomial ue with
        | Some p -> (Guard.mk_true, p)
        | None ->
            let v' = Var.fresh_id Var.Int () in
            (UpdateElement_.as_guard ue v', Polynomial.of_var v'))
      update_map
    |> fun m -> (Guard.all (List.map ~f:Tuple2.first @@ Map.data m), Map.map ~f:Tuple2.second m)


  let add_self_updates_if_missing to_update_value vars update_map =
    Set.fold
      ~f:(fun fold_update var ->
        if Map.mem update_map var then
          fold_update
        else
          Map.add_exn fold_update ~key:var ~data:(to_update_value var))
      vars ~init:update_map


  let fill_up_update_arg_vars_up_to_num from_var n update =
    let all_args = VarSet.of_sequence @@ Sequence.take Var.args n in
    add_self_updates_if_missing from_var all_args update


  let fill_up_arg_vars_up_to_num n t =
    let properties = t.properties in
    let properties =
      {
        properties with
        update = fill_up_update_arg_vars_up_to_num UpdateElement_.of_var n properties.update;
        overappr_nonprob_update =
          fill_up_update_arg_vars_up_to_num Polynomial.of_var n properties.overappr_nonprob_update;
      }
    in
    { t with properties }


  (** Creates a new probabilistic transition. Sets its general transition to undefined, i.e., [None]*)
  let mk ~patterns ~map_to_arg_vars ~probability ~assignments ~guard =
    let update =
      Sequence.of_list assignments
      |> Sequence.map ~f:(UpdateElement_.rename map_to_arg_vars)
      |> Sequence.zip Var.args
      |> Map.of_sequence_exn (module Var)
      |> fill_up_update_arg_vars_up_to_num UpdateElement_.of_var (List.length patterns)
    in

    let guard = Guard.rename guard map_to_arg_vars in
    let overappr_guard, overappr_nonprob_update = compute_overapproximated_update_and_guard update in
    let overappr_guard = Guard.mk_and overappr_guard guard in
    {
      id = Unique.unique ();
      gt = ref None;
      properties = { probability; overappr_guard; update; overappr_nonprob_update };
    }


  (** Similar to [mk] but higher level*)
  let mk_2 properties = { id = Unique.unique (); gt = ref None; properties }

  let comparator = LabelComparator.comparator

  type comparator_witness = LabelComparator.comparator_witness

  let compare = Comparator.compare_of_comparator comparator
  let equal = Comparator.equal_of_comparator comparator
  let id t = t.id
  let gt t = Option.value_exn !(t.gt)
  let gt_id t = (gt t).gt_id
  let probability t = t.properties.probability

  (** In contrast to ProbabilisticTransitionLabel.guard this returns the guard {b without} invariant *)
  let guard t = (gt t).guard

  let overappr_guard t = t.properties.overappr_guard
  let update_map t = t.properties.update
  let overappr_nonprob_update t = t.properties.overappr_nonprob_update
  let invariant t = (gt t).invariant
  let cost t = (gt t).cost
  let without_backlink t = t.properties

  (** Returns a string representing the left hand side of the update function. Parameter {i to_file} is used to get a representation with less special characters. *)
  let update_to_string_lhs_ ~pretty t =
    let update = update_map t in
    if Map.is_empty update then
      ""
    else
      Map.to_alist update |> List.map ~f:(fun (var, _) -> Var.to_string ~pretty var) |> fun xs ->
      "(" ^ String.concat ~sep:"," xs ^ ")"


  let update_to_string_lhs = update_to_string_lhs_ ~pretty:false
  let update_to_string_lhs_pretty = update_to_string_lhs_ ~pretty:true

  let update_to_string_rhs_ ue_to_string get_update t =
    let update = get_update t in
    if Map.is_empty update then
      ""
    else
      Map.to_alist update |> List.map ~f:(fun (_, poly) -> ue_to_string poly) |> fun xs ->
      "(" ^ String.concat ~sep:"," xs ^ ")"


  let update_to_string_rhs ue_to_string get_update = update_to_string_rhs_ ue_to_string get_update

  let update_to_string_rhs_pretty ue_to_string_pretty get_update =
    update_to_string_rhs_ ue_to_string_pretty get_update


  let update_to_string ue_to_string update =
    if Map.is_empty update then
      "true"
    else
      Map.to_alist update
      |> List.map ~f:(fun (var, poly) -> (Var.to_string var, ue_to_string poly))
      |> List.unzip
      |> fun (xs, ys) -> "(" ^ String.concat ~sep:"," xs ^ ") -> (" ^ String.concat ~sep:"," ys ^ ")"


  let to_string ue_to_string ue_to_string_pretty get_update get_guard ?(pretty = false) t =
    let guard =
      if Guard.is_true (get_guard t) then
        ""
      else
        " :|: " ^ Guard.to_string ~pretty (get_guard t)
    in
    let invariant =
      if Invariant.is_true (invariant t) then
        ""
      else
        " [" ^ Guard.to_string ~pretty (invariant t) ^ "] "
    in
    let cost =
      if Polynomial.is_one (cost t) then
        if pretty then
          "->"
        else
          ""
      else if pretty then
        "-{" ^ Polynomial.to_string_pretty (cost t)
      else
        Polynomial.to_string (cost t) ^ "}>"
    in
    if pretty then
      RationalLaurentPolynomial.to_string (probability t)
      ^ ":" ^ "t" ^ Util.natural_to_subscript t.id ^ "∈g"
      ^ Util.natural_to_subscript (gt_id t)
      ^ ":" ^ invariant ^ " " ^ update_to_string_lhs_pretty t ^ " " ^ cost ^ " "
      ^ update_to_string_rhs_pretty ue_to_string_pretty get_update t
      ^ guard
    else
      "ID: "
      ^ RationalLaurentPolynomial.to_string (probability t)
      ^ ":" ^ invariant
      ^ string_of_int (gt_id t)
      ^ "," ^ string_of_int t.id ^ ", " ^ cost ^ "&euro;" ^ ", "
      ^ update_to_string ue_to_string (get_update t)
      ^ guard


  let cost_to_string t =
    if Polynomial.is_one (cost t) then
      ""
    else
      "{" ^ Polynomial.to_string (cost t) ^ "}"


  let to_id_string t = string_of_int (gt_id t) ^ "," ^ string_of_int t.id
  let input_vars t = VarSet.of_list @@ Map.keys (update_map t)
  let input_size = Set.length % input_vars

  type vars_type = VarsOverapproximated | VarsNonOverapproximated

  let vars vars_type t =
    let update_and_guard_vars =
      match vars_type with
      | VarsOverapproximated ->
          Set.union
            (Map.fold
               ~f:(fun ~key ~data -> Set.union (Polynomial.vars data))
               (overappr_nonprob_update t) ~init:VarSet.empty)
            (Guard.vars (overappr_guard t))
      | VarsNonOverapproximated ->
          Set.union
            (Map.fold
               ~f:(fun ~key ~data -> Set.union (UpdateElement_.vars data))
               (update_map t) ~init:VarSet.empty)
            (Guard.vars (guard t))
    in
    update_and_guard_vars
    |> Set.union (Guard.vars (invariant t))
    |> Set.union (Polynomial.vars (cost t))
    |> Set.union (VarSet.of_list @@ Map.keys (update_map t))


  let rename_update rename_ue update rename_map =
    update |> Map.to_sequence
    |> Sequence.map ~f:(fun (key, value) ->
           (RenameMap.find key rename_map ~default:key, rename_ue rename_map value))
    |> Map.of_sequence_exn (module Var)


  (** Use with caution as you may also want to rename guards & invariants of the containing general transition *)
  let rename rename_map t =
    {
      t with
      properties =
        {
          probability = RationalLaurentPolynomial.rename rename_map (probability t);
          update = rename_update UpdateElement_.rename (update_map t) rename_map;
          overappr_nonprob_update = rename_update Polynomial.rename (overappr_nonprob_update t) rename_map;
          overappr_guard = Guard.rename (overappr_guard t) rename_map;
        };
    }


  let update_admissibility_constraint t : Guard.t =
    Map.to_sequence (update_map t)
    |> Sequence.map ~f:(UpdateElement_.admissibility_constraint % Tuple2.second)
    |> Sequence.fold ~f:Guard.mk_and ~init:Guard.mk_true


  let chain_guards t1 t2 =
    let nondet_vars = Hashtbl.create ~size:3 (module Var) in
    let substitution update_map var =
      Map.find update_map var
      |? Polynomial.of_var
           (* Variables which are nondeterministic in the preceding transition are represented by fresh variables. *)
           (Hashtbl.find nondet_vars var
           |> Option.value_or_thunk ~default:(fun () ->
                  let nondet_var = Var.fresh_id Var.Int () in
                  Hashtbl.add_exn nondet_vars ~key:var ~data:nondet_var;
                  nondet_var))
    in
    Guard.Infix.(
      overappr_guard t1 && invariant t1
      && Guard.map_polynomial
           (Polynomial.substitute_f (substitution (overappr_nonprob_update t1)))
           (overappr_guard t2)
      && Guard.map_polynomial
           (Polynomial.substitute_f (substitution (overappr_nonprob_update t1)))
           (invariant t2))


  let restore_legacy_distribution_update_semantics t =
    let update =
      Map.mapi
        ~f:(fun ~key ~data -> UpdateElement_.restore_legacy_distribution_update_semantics key data)
        (update_map t)
    in
    let overappr_guard, overappr_nonprob_update =
      Map.map
        ~f:(fun ue ->
          match UpdateElement_.to_polynomial ue with
          | Some p -> (Guard.mk_true, p)
          | None ->
              let v' = Var.fresh_id Var.Int () in
              (UpdateElement_.as_guard ue v', Polynomial.of_var v'))
        update
      |> fun m -> (Guard.all (List.map ~f:Tuple2.first @@ Map.data m), Map.map ~f:Tuple2.second m)
    in
    { t with properties = { t.properties with update; overappr_guard; overappr_nonprob_update } }
end

module ProbabilisticTransitionLabel = struct
  include ProbabilisticTransitionLabel_

  type update_element = UpdateElement_.t

  let update_map t = t.properties.update
  let update t v = Map.find (update_map t) v
  let guard_without_inv t = guard t
  let guard t = Guard.mk_and (invariant t) (guard t)

  (** Returns if the two labels describe the same transition *)
  let equivalent t1 t2 =
    Map.equal UpdateElement_.equal (update_map t1) (update_map t2)
    && RationalLaurentPolynomial.equal (probability t1) (probability t2)
    && Guard.equal (guard t1) (guard t2)
    && Invariant.equal (invariant t1) (invariant t2)
    && Polynomial.equal (cost t1) (cost t2)


  let compare_equivalent t1 t2 =
    if RationalLaurentPolynomial.compare (probability t1) (probability t2) != 0 then
      RationalLaurentPolynomial.compare (probability t1) (probability t2)
    else if Map.compare_direct UpdateElement_.compare (update_map t1) (update_map t2) != 0 then
      Map.compare_direct UpdateElement_.compare (update_map t1) (update_map t2)
    else if Guard.compare (guard t1) (guard t2) != 0 then
      Guard.compare (guard t1) (guard t2)
    else if Invariant.compare (invariant t1) (invariant t2) != 0 then
      Invariant.compare (invariant t1) (invariant t2)
    else if Polynomial.compare (cost t1) (cost t2) != 0 then
      Polynomial.compare (cost t1) (cost t2)
    else
      0


  let equivalent_update t1 t2 = Map.equal UpdateElement_.equal (update_map t1) (update_map t2)

  let ids_to_string ?(pretty = false) t =
    if pretty then
      "t" ^ Util.natural_to_subscript (id t) ^ " ∈ g" ^ Util.natural_to_subscript (gt_id t)
    else
      "t" ^ Int.to_string (id t) ^ " in g" ^ Int.to_string (gt_id t)


  let update_to_string_rhs = update_to_string_rhs UpdateElement_.to_string update_map
  let update_to_string_rhs_pretty = update_to_string_rhs_pretty UpdateElement_.to_string_pretty update_map

  let to_string =
    to_string UpdateElement_.to_string UpdateElement_.to_string_pretty (fun t -> update_map t) guard


  let vars = vars VarsNonOverapproximated
  let has_tmp_vars t = not @@ Set.is_empty @@ Set.diff (vars t) (input_vars t)
  let tmp_vars t = Set.diff (vars t) (input_vars t)

  let changed_vars t =
    input_vars t |> Set.filter ~f:(fun v -> not UpdateElement_.(equal (of_var v) (update t v |? of_var v)))


  let negative_costs t =
    SMT.Z3Solver.satisfiable Formula.(mk_and (mk @@ guard t) (mk_gt Polynomial.zero (cost t)))


  let remove_non_contributors non_contributors t =
    let update_ = Set.fold ~f:(fun u var -> Map.remove u var) non_contributors ~init:(update_map t) in
    let guard_ =
      List.filter
        ~f:(fun atom -> Set.are_disjoint non_contributors (Polynomial.vars @@ Atoms.Atom.poly atom))
        (Guard.atom_list t.properties.overappr_guard)
    in
    { t with properties = { t.properties with update = update_; overappr_guard = guard_ } }
end

module ProbabilisticTransitionLabelNonProbOverappr = struct
  include ProbabilisticTransitionLabel_

  type update_element = Polynomial.t

  let map_guard f t = { t with properties = { t.properties with overappr_guard = f (overappr_guard t) } }
  let update_map t = overappr_nonprob_update t
  let update t v = Map.find (overappr_nonprob_update t) v
  let guard_without_inv t = overappr_guard t
  let guard t = Guard.mk_and (invariant t) (overappr_guard t)

  (** Returns if the two labels describe the same transition *)
  let equivalent t1 t2 =
    Map.equal Polynomial.equal (overappr_nonprob_update t1) (overappr_nonprob_update t2)
    && Guard.equal (overappr_guard t1) (overappr_guard t2)
    && Invariant.equal (invariant t1) (invariant t2)
    && Polynomial.equal (cost t1) (cost t2)


  let compare_equivalent t1 t2 =
    if Map.compare_direct Polynomial.compare (overappr_nonprob_update t1) (overappr_nonprob_update t2) != 0
    then
      Map.compare_direct Polynomial.compare (overappr_nonprob_update t1) (overappr_nonprob_update t2)
    else if Guard.compare (overappr_guard t1) (overappr_guard t2) != 0 then
      Guard.compare (overappr_guard t1) (overappr_guard t2)
    else if Invariant.compare (invariant t1) (invariant t2) != 0 then
      Invariant.compare (invariant t1) (invariant t2)
    else if Polynomial.compare (cost t1) (cost t2) != 0 then
      Polynomial.compare (cost t1) (cost t2)
    else
      0


  let equivalent_update t1 t2 =
    Map.equal Polynomial.equal (overappr_nonprob_update t1) (overappr_nonprob_update t2)


  let ids_to_string ?(pretty = false) t =
    if pretty then
      "t" ^ Util.natural_to_subscript (id t)
    else
      "t" ^ Int.to_string (id t)


  let update_to_string_rhs = update_to_string_rhs Polynomial.to_string overappr_nonprob_update

  let update_to_string_rhs_pretty =
    update_to_string_rhs_pretty Polynomial.to_string_pretty overappr_nonprob_update


  let to_string =
    to_string Polynomial.to_string Polynomial.to_string_pretty overappr_nonprob_update overappr_guard


  let vars = vars VarsOverapproximated
  let tmp_vars t = Set.diff (vars t) (input_vars t)

  let relax_guard ~non_static t =
    let is_static atom = Set.is_subset (Atoms.Atom.vars atom) ~of_:(Set.diff (input_vars t) non_static) in
    { t with properties = { t.properties with overappr_guard = List.filter ~f:is_static (overappr_guard t) } }


  let has_tmp_vars t = not @@ Set.is_empty @@ Set.diff (vars t) (input_vars t)

  let negative_costs t =
    SMT.Z3Solver.satisfiable Formula.(mk_and (mk @@ guard t) (mk_gt Polynomial.zero (cost t)))


  let changed_vars t =
    input_vars t
    |> Set.filter ~f:(fun v ->
           not Polynomial.(equal (of_var v) (Map.find (overappr_nonprob_update t) v |? of_var v)))


  let remove_non_contributors non_contributors t =
    let update_ =
      Set.fold ~f:(fun u var -> Map.remove u var) non_contributors ~init:(overappr_nonprob_update t)
    in
    { t with properties = { t.properties with overappr_nonprob_update = update_ } }


  let overapprox_nonlinear_updates t =
    let handle_nonlinear_or_probabilistic ue =
      let v' = Var.fresh_id Var.Int () in
      (Polynomial.of_var v', UpdateElement_.as_linear_guard (guard t) ue v')
    in
    let overappr_nonprob_update, overappr_guard_ =
      Map.map
        ~f:(fun ue ->
          match UpdateElement_.to_polynomial ue with
          | Some p when Polynomial.degree p = 1 -> (p, Guard.mk_true)
          | _ -> handle_nonlinear_or_probabilistic ue)
        (ProbabilisticTransitionLabel.update_map t)
      |> fun m -> (Map.map ~f:Tuple2.first m, Guard.all (List.map ~f:Tuple2.second @@ Map.data m))
    in
    let overappr_guard_ = Guard.mk_and overappr_guard_ (Guard.drop_nonlinear (overappr_guard t)) in
    { t with properties = { t.properties with overappr_nonprob_update; overappr_guard = overappr_guard_ } }
end

(** Shared definitions between ProbabilisticTransition and ProbabilisticTransitionNonProbOverAppr *)
module ProbabilisticTransitionShared = struct
  module Inner = struct
    (* do not use include here so that we have to manually check changes in TransitionOver *)
    module GenericTransition = Transition_.Make (ProbabilisticTransitionLabel)

    type t = GenericTransition.t
    type transition_label = GenericTransition.transition_label
    type transition_label_comparator_witness = GenericTransition.transition_label_comparator_witness

    let src = GenericTransition.src
    let label = GenericTransition.label
    let target = GenericTransition.target
    let id = GenericTransition.id
    let gt_id (_, label, _) = ProbabilisticTransitionLabel.gt_id label
    let compare = GenericTransition.compare
    let equal = GenericTransition.equal
    let equivalent = GenericTransition.equivalent
    let compare_equivalent = GenericTransition.compare_equivalent
    let hash = GenericTransition.hash
    let map_label = GenericTransition.map_label
    let cost = GenericTransition.cost
    let without_backlink (l, label, l') = (l, ProbabilisticTransitionLabel.without_backlink label, l')
  end

  include Inner

  type comparator_witness = transition_label_comparator_witness TransitionComparator.comparator_witness

  let comparator = TransitionComparator.comparator ProbabilisticTransitionLabel.comparator
  let sexp_of_t = Sexplib0.Sexp_conv.sexp_of_opaque
end

module ProbabilisticTransition = struct
  include ProbabilisticTransitionShared

  let gt (_, label, _) = ProbabilisticTransitionLabel.gt label
  let vars (_, label, _) = ProbabilisticTransitionLabel.vars label
  let input_vars (_, label, _) = ProbabilisticTransitionLabel.input_vars label

  let to_string (l, t, l') =
    Int.to_string (ProbabilisticTransitionLabel.gt_id t)
    ^ ":"
    ^ Int.to_string (ProbabilisticTransitionLabel.id t)
    ^ ":" ^ Location.to_string l
    ^ ProbabilisticTransitionLabel.update_to_string_lhs t
    ^ " -"
    ^ ProbabilisticTransitionLabel.cost_to_string t
    ^ "> " ^ Location.to_string l'
    ^ ProbabilisticTransitionLabel.update_to_string_rhs t
    ^
    if Constraints.Constraint.is_true (ProbabilisticTransitionLabel.guard t) then
      ""
    else
      ":|:" ^ Guard.to_string (ProbabilisticTransitionLabel.guard t)


  let to_string_pretty (l, t, l') =
    "t"
    ^ Util.natural_to_subscript (ProbabilisticTransitionLabel.id t)
    ^ "∈g"
    ^ Util.natural_to_subscript (ProbabilisticTransitionLabel.gt_id t)
    ^ ": " ^ Location.to_string l
    ^ ProbabilisticTransitionLabel.update_to_string_lhs_pretty t
    ^ (if Polynomials.Polynomial.(equal one (ProbabilisticTransitionLabel.cost t)) then
         " → "
       else
         " -" ^ ProbabilisticTransitionLabel.cost_to_string t ^ "> ")
    ^ Location.to_string l'
    ^ ProbabilisticTransitionLabel.update_to_string_rhs_pretty t
    ^
    if Constraints.Constraint.is_true (ProbabilisticTransitionLabel.guard t) then
      ""
    else
      " :|: " ^ Guard.to_string ~pretty:true (ProbabilisticTransitionLabel.guard t)


  let to_id_string (l, label, l') =
    Int.to_string (ProbabilisticTransitionLabel.gt_id label)
    ^ ":"
    ^ Int.to_string (ProbabilisticTransitionLabel.id label)
    ^ ": " ^ Location.to_string l ^ "->"
    ^ RationalLaurentPolynomial.to_string (ProbabilisticTransitionLabel.probability label)
    ^ ":" ^ Location.to_string l'


  let to_id_string_pretty (l, label, l') =
    "t"
    ^ Util.natural_to_subscript (ProbabilisticTransitionLabel.id label)
    ^ "∈g"
    ^ Util.natural_to_subscript (ProbabilisticTransitionLabel.gt_id label)
    ^ ": " ^ Location.to_string l ^ "→"
    ^ RationalLaurentPolynomial.to_string (ProbabilisticTransitionLabel.probability label)
    ^ ":" ^ Location.to_string l'


  let to_file_string_rhs (_, label, l') =
    let prob = ProbabilisticTransitionLabel.probability label in
    let prob_string =
      if RationalLaurentPolynomial.(equal one prob) then
        ""
      else
        "[" ^ RationalLaurentPolynomial.to_string prob ^ "]:"
    in
    prob_string ^ Location.to_string l' ^ ProbabilisticTransitionLabel.update_to_string_rhs label


  let to_string_rhs (_, label, l') =
    let prob = ProbabilisticTransitionLabel.probability label in
    let prob_string =
      if RationalLaurentPolynomial.(equal one prob) then
        ""
      else
        "[" ^ RationalLaurentPolynomial.to_string prob ^ "]:"
    in
    prob_string
    ^ (Int.to_string (ProbabilisticTransitionLabel.id label) ^ ":")
    ^ Location.to_string l'
    ^ ProbabilisticTransitionLabel.update_to_string_rhs label


  let to_string_rhs_pretty (_, label, l') =
    let prob = ProbabilisticTransitionLabel.probability label in
    let prob_string =
      if RationalLaurentPolynomial.(equal one prob) then
        ""
      else
        "[" ^ RationalLaurentPolynomial.to_string prob ^ "]:"
    in
    prob_string
    ^ ("t" ^ Util.natural_to_subscript (ProbabilisticTransitionLabel.id label) ^ ":")
    ^ Location.to_string l'
    ^ ProbabilisticTransitionLabel.update_to_string_rhs_pretty label


  let restore_legacy_distribution_update_semantics (l, label, l') =
    (l, ProbabilisticTransitionLabel.restore_legacy_distribution_update_semantics label, l')


  let same_gt t1 t2 = Int.equal (gt_id t1) (gt_id t2)
end

module ProbabilisticTransitionNonProbOverappr = struct
  module GenericClassical = Transition_.MakeClassical (ProbabilisticTransitionLabelNonProbOverappr)
  include ProbabilisticTransitionShared

  let to_id_string = GenericClassical.to_id_string
  let to_id_string_pretty = GenericClassical.to_id_string_pretty
  let to_string = GenericClassical.to_string
  let to_string_pretty = GenericClassical.to_string_pretty
  let overapprox_nonlinear_updates = GenericClassical.overapprox_nonlinear_updates
end

module GeneralTransition = struct
  module ProbabilisticTransitionSet = Transition_.TransitionSetOver (ProbabilisticTransition)

  module Inner = struct
    type t = general_transition

    let sexp_of_t = Sexplib0.Sexp_conv.sexp_of_opaque
    let get_arbitrary_transition t = Set.choose_exn t.transitions
    let get_arbitrary_label = ProbabilisticTransition.label % get_arbitrary_transition
    (* TODO conjunction with invariant! *)

    let guard t = Guard.mk_and t.guard t.invariant
    let map_transitions f gt = { gt with transitions = ProbabilisticTransitionSet.map ~f gt.transitions }

    let map_gt f (gt : t) =
      let gt = f gt in
      (* This should be enough to create transitions with fresh gt references so we don't alter the original transitions *)
      let transitions =
        ProbabilisticTransitionSet.map ~f:(fun (l, t, l') -> (l, { t with gt = ref None }, l')) gt.transitions
      in
      let gt = { gt with transitions } in
      (* Set references to new general transition *)
      Set.iter gt.transitions ~f:(fun t -> (ProbabilisticTransition.label t).gt := Some gt);
      gt


    let add_invariant (gt : t) invariant : t =
      let invariant =
        let guard_atoms = Guard.to_set gt.guard in
        let invariant_atoms = Guard.mk_and gt.invariant invariant |> Guard.to_set in
        Set.diff invariant_atoms guard_atoms |> Guard.of_set
      in
      map_gt (fun gt -> { gt with invariant }) gt


    let simplify_guard = map_gt (fun gt -> { gt with guard = Guard.simplify_guard gt.guard })

    let mk ~(start : Location.t) ~(fill_up_to_num_arg_vars : int) ~(patterns : Var.t list)
        ~(cost : Polynomials.Polynomial.t) ~(guard : Guard.t)
        ~(rhss : (RationalLaurentPolynomial.t * UpdateElement_.t list * Location.t) list) : t =
      let total_prob =
        Sequence.of_list rhss |> Sequence.map ~f:Tuple3.first |> RationalLaurentPolynomial.sum
      in
      if not (RationalLaurentPolynomial.equal RationalLaurentPolynomial.one total_prob) then
        raise ProbabilitiesDoNotSumToOne;

      let map_to_arg_vars = Sequence.zip (Sequence.of_list patterns) Var.args |> RenameMap.of_sequence in

      let gt_id = Unique.unique () in
      let rhss =
        List.map
          ~f:(fun (p, ues, l) ->
            ( ProbabilisticTransitionLabel.fill_up_arg_vars_up_to_num fill_up_to_num_arg_vars
              @@ ProbabilisticTransitionLabel_.mk ~map_to_arg_vars ~patterns ~probability:p ~assignments:ues
                   ~guard,
              l ))
          rhss
      in

      (* add update_admissibility constraints *)
      let update_admissibility =
        Sequence.map ~f:Tuple2.first (Sequence.of_list rhss)
        |> Sequence.map ~f:ProbabilisticTransitionLabel.update_admissibility_constraint
        |> Sequence.fold ~f:Guard.mk_and ~init:Guard.mk_true
      in

      (* rename program vars to arg vars *)
      let rename_map = RenameMap.of_sequence @@ Sequence.zip (Sequence.of_list patterns) Var.args in
      let rhss = List.map ~f:(Tuple2.map1 (ProbabilisticTransitionLabel_.rename rename_map)) rhss in
      let guard = Guard.remove_duplicate_atoms (Guard.rename guard map_to_arg_vars) in
      let gt =
        {
          gt_id;
          guard;
          cost = Polynomial.rename map_to_arg_vars cost;
          invariant = update_admissibility;
          transitions =
            ProbabilisticTransitionSet.of_list
            @@ List.map ~f:(fun (label, target) -> (start, label, target)) rhss;
        }
      in
      (* Tying the knot *)
      Set.iter ~f:(fun (l, t, l') -> t.gt := Some gt) gt.transitions;
      gt


    let mk_from_labels_without_backlink ~start ~guard_without_invariant ~invariant ~cost ~rhss : t =
      let total_prob =
        Sequence.of_list rhss
        |> Sequence.map ~f:(fun (r, _) -> r.probability)
        |> RationalLaurentPolynomial.sum
      in
      if not (RationalLaurentPolynomial.equal RationalLaurentPolynomial.one total_prob) then
        raise ProbabilitiesDoNotSumToOne;

      let gt =
        {
          gt_id = Unique.unique ();
          cost;
          guard = Guard.remove_duplicate_atoms guard_without_invariant;
          invariant = Guard.mk_true;
          transitions =
            Sequence.of_list rhss
            |> Sequence.map ~f:(fun (rhs, l') ->
                   let label = ProbabilisticTransitionLabel_.mk_2 rhs in
                   (start, label, l'))
            |> ProbabilisticTransitionSet.of_sequence;
        }
        |> flip add_invariant invariant
      in
      (* Tying the knot *)
      Set.iter ~f:(fun (l, t, l') -> t.gt := Some gt) gt.transitions;
      gt


    let gt_id = ProbabilisticTransitionLabel.gt_id % get_arbitrary_label
    let transitions t = t.transitions

    let transitions_to_target target t =
      Set.filter t.transitions ~f:(Location.equal target % ProbabilisticTransition.target)


    let cost (gt : t) = gt.cost
    let src = ProbabilisticTransition.src % get_arbitrary_transition
    let invariant gt = gt.invariant
    let guard_without_inv gt = gt.guard

    let targets t =
      Set.of_sequence (module Location)
      @@ Sequence.map ~f:ProbabilisticTransition.target (Set.to_sequence t.transitions)


    let equal t1 t2 = Int.equal (gt_id t1) (gt_id t2)
    let compare t1 t2 = Int.compare (gt_id t1) (gt_id t2)
    let hash = Hashtbl.hash % gt_id

    let vars t =
      Set.to_sequence t.transitions
      |> Sequence.map ~f:ProbabilisticTransition.vars
      |> Sequence.fold ~f:Set.union ~init:VarSet.empty


    let input_vars t =
      Set.to_sequence t.transitions
      |> Sequence.map ~f:ProbabilisticTransition.input_vars
      |> Sequence.fold ~f:Set.union ~init:VarSet.empty


    let locations t = Set.add (targets t) (src t)

    let is_probabilistic t =
      let transs = transitions t in
      match Set.choose transs with
      | Some trans when Set.length transs = 1 ->
          let update_map = ProbabilisticTransitionLabel.update_map (ProbabilisticTransition.label trans) in
          Map.exists update_map ~f:UpdateElement_.is_probabilistic
      | _ -> true


    let to_id_string t =
      let show_prob_branch (_, label, l') =
        Int.to_string (ProbabilisticTransitionLabel.id label)
        ^ ": "
        ^ RationalLaurentPolynomial.to_string (ProbabilisticTransitionLabel.probability label)
        ^ ":" ^ Location.to_string l'
      in
      Int.to_string (gt_id t)
      ^ ":"
      ^ Location.to_string (src t)
      ^ "->"
      ^ Util.sequence_to_string ~f:show_prob_branch (Set.to_sequence t.transitions)


    let to_id_string_pretty t =
      let show_prob_branch (_, label, l') =
        "t"
        ^ Util.natural_to_subscript (ProbabilisticTransitionLabel.id label)
        ^ ":"
        ^ RationalLaurentPolynomial.to_string (ProbabilisticTransitionLabel.probability label)
        ^ ":" ^ Location.to_string l'
      in
      "g"
      ^ Util.natural_to_subscript (gt_id t)
      ^ ":"
      ^ Location.to_string (src t)
      ^ "→"
      ^ Util.sequence_to_string ~f:show_prob_branch (Set.to_sequence t.transitions)


    let to_string t =
      let id_string = Int.to_string (gt_id t) ^ ": " in
      let vars =
        ProbabilisticTransitionLabel.update_to_string_lhs
        @@ ProbabilisticTransition.label (Set.choose_exn t.transitions)
      in
      let trans = Set.to_sequence t.transitions in
      let cost_str =
        if Polynomial.(equal (cost t) one) then
          ""
        else
          "{" ^ Polynomial.to_string (cost t) ^ "}"
      in
      let trans_string =
        Sequence.map ~f:ProbabilisticTransition.to_string_rhs trans
        |> Sequence.reduce ~f:(fun a b -> a ^ " :+: " ^ b)
        |? ""
      in
      id_string
      ^ Location.to_string (src t)
      ^ vars ^ " -" ^ cost_str ^ "> " ^ trans_string ^ " :|: "
      ^ Guard.to_string ~pretty:false (guard t)


    let to_file_string t =
      let vars =
        ProbabilisticTransitionLabel.update_to_string_lhs
        @@ ProbabilisticTransition.label (Set.choose_exn t.transitions)
      in
      let trans = Set.to_sequence t.transitions in
      let cost_str =
        if Polynomial.(equal (cost t) one) then
          ""
        else
          "{" ^ Polynomial.to_string (cost t) ^ "}"
      in
      let trans_string =
        Sequence.map ~f:ProbabilisticTransition.to_file_string_rhs trans
        |> Sequence.reduce_exn ~f:(fun a b -> a ^ " :+: " ^ b)
      in
      Location.to_string (src t)
      ^ vars ^ " -" ^ cost_str ^ "> " ^ trans_string
      ^
      if Guard.is_true (guard t) then
        ""
      else
        " :|: " ^ Guard.to_string ~pretty:false (guard t)


    let to_string_pretty t =
      let id_string = "g" ^ Util.natural_to_subscript (gt_id t) ^ ":" in
      let vars =
        ProbabilisticTransitionLabel.update_to_string_lhs_pretty
        @@ ProbabilisticTransition.label (Set.choose_exn t.transitions)
      in
      let trans = Set.to_sequence t.transitions in
      let cost_str =
        if Polynomial.(equal (cost t) one) then
          ""
        else
          "{" ^ Polynomial.to_string_pretty (cost t) ^ "}"
      in
      let trans_string =
        Sequence.map ~f:ProbabilisticTransition.to_string_rhs_pretty trans
        |> Sequence.reduce ~f:(fun a b -> a ^ " :+: " ^ b)
        |? ""
      in
      let arrow =
        if String.is_empty cost_str then
          "→"
        else
          "-" ^ cost_str ^ ">"
      in
      id_string
      ^ Location.to_string (src t)
      ^ vars ^ " " ^ arrow ^ " " ^ trans_string ^ " :|: "
      ^ Guard.to_string ~pretty:true (guard t)


    let ids_to_string ?(pretty = false) gt =
      if pretty then
        "g" ^ Util.natural_to_subscript (gt_id gt)
      else
        "g" ^ Int.to_string (gt_id gt)
  end

  include Inner
  include Comparator.Make (Inner)
end

module GeneralTransitionSet = struct
  module GeneralTransition = GeneralTransition
  include MakeSetCreators0 (GeneralTransition)

  let to_string = Util.sequence_to_string ~f:GeneralTransition.to_id_string % Set.to_sequence
  let to_id_string = Util.sequence_to_string ~f:GeneralTransition.to_id_string % Set.to_sequence
  let to_id_string_pretty = Util.sequence_to_string ~f:GeneralTransition.to_id_string_pretty % Set.to_sequence
  let of_tset = map ~f:ProbabilisticTransition.gt

  let locations t =
    Set.fold
      ~f:(fun set gt -> Set.add set (GeneralTransition.src gt) |> Set.union (GeneralTransition.targets gt))
      t
      ~init:(Set.empty (module Location))


  let targets gts =
    Set.to_sequence gts
    |> Sequence.map ~f:GeneralTransition.targets
    |> Sequence.fold ~f:Set.union ~init:(Set.empty (module Location))


  let all_transitions t =
    Sequence.map ~f:(Set.to_sequence % GeneralTransition.transitions) (Set.to_sequence t)
    |> Set.of_sequence (module ProbabilisticTransition) % Sequence.join


  let find_by_id set id =
    Set.binary_search set ~compare:(fun t i -> Int.compare (GeneralTransition.gt_id t) i) `First_equal_to id


  let find_by_ids set ids =
    Sequence.map ~f:(find_by_id set) ids |> Sequence.filter_opt |> Set.of_sequence (module GeneralTransition)
end

(* Probabilistic and Overapproximated Nonprobabilistic Programs share the same internal *)
(* TransitionGraph representation to allow for an efficient conversion between both *)
(* Here we use the internal ProbabilisticTransitionLabel_ *)
module TransitionGraph_Ocamlgraph_Repr_ =
  Graph.Persistent.Digraph.ConcreteBidirectionalLabeled (Location) (ProbabilisticTransitionLabel_)

module ProbabilisticTransitionGraph = struct
  include TransitionGraph_.Make_ (ProbabilisticTransition) (TransitionGraph_Ocamlgraph_Repr_)

  let gts = GeneralTransitionSet.of_tset % transitions

  let map_gtsset ~f (gts : GeneralTransitionSet.t) (t : t) : t =
    (* Here, after applying f to the general transitions in gts the transitions contained in the original graph will point back to the wrong general transition.
       Hence, we replace them here. *)
    let graph = GeneralTransitionSet.all_transitions gts |> Set.fold ~init:t ~f:remove_edge_e in
    let gts = GeneralTransitionSet.map gts ~f in
    GeneralTransitionSet.all_transitions gts |> Set.fold ~init:graph ~f:add_edge_e


  let add_invariant location invariant t =
    let out_gts =
      Sequence.of_list (succ_e t location)
      |> Sequence.map ~f:ProbabilisticTransition.gt
      |> GeneralTransitionSet.of_sequence
    in
    map_gtsset out_gts ~f:(fun gt -> GeneralTransition.add_invariant gt invariant) t


  let outgoing_gts t location =
    Sequence.of_list (succ_e t location)
    |> Sequence.map ~f:ProbabilisticTransition.gt
    |> GeneralTransitionSet.of_sequence
end

module ProbabilisticProgram = struct
  include
    Program_.Make (ProbabilisticTransitionLabel) (ProbabilisticTransition) (ProbabilisticTransitionGraph)

  let pre_gt t gt =
    (* All transitions in a gt have the same guard and hence the same pre transitions *)
    let pre_t = pre t (Set.choose_exn @@ GeneralTransition.transitions gt) in

    GeneralTransitionSet.map pre_t ~f:ProbabilisticTransition.gt


  let succ_gts program loc =
    transitions_from_location program loc |> GeneralTransitionSet.map ~f:ProbabilisticTransition.gt


  let from_gts (start : Location.t) gts =
    Set.to_sequence (GeneralTransitionSet.all_transitions gts) |> from_sequence start


  let restore_legacy_distribution_update_semantics =
    map_transitions ProbabilisticTransition.restore_legacy_distribution_update_semantics


  let tmp_vars t = Set.diff (vars t) (input_vars t)
  let is_initial_gt t gt = Location.equal (start t) (GeneralTransition.src gt)
  let remove_unsatisfiable_transitions t = Set.fold ~init:t ~f:remove_transition
  let add_invariant loc inv = map_graph (ProbabilisticTransitionGraph.add_invariant loc inv)

  let map_gtsset ~f (gts : GeneralTransitionSet.t) (t : t) : t =
    map_graph (ProbabilisticTransitionGraph.map_gtsset ~f gts) t


  let gts = ProbabilisticTransitionGraph.gts % graph
  let simplify_all_guards (t : t) : t = map_gtsset (gts t) ~f:GeneralTransition.simplify_guard t

  let sccs_gts t =
    sccs t
    |> List.map ~f:(fun tset ->
           Set.to_sequence tset
           |> Sequence.map ~f:ProbabilisticTransition.gt
           |> GeneralTransitionSet.of_sequence)


  let scc_gts_from_locs program locs =
    let transitions =
      Set.to_sequence locs
      |> Sequence.map ~f:(fun loc ->
             Sequence.of_list (ProbabilisticTransitionGraph.succ_e (graph program) loc)
             |> Sequence.filter ~f:(Set.mem locs % ProbabilisticTransition.target))
      |> Sequence.join
    in
    Sequence.map ~f:ProbabilisticTransition.gt transitions |> GeneralTransitionSet.of_sequence


  let scc_gts_from_locs_with_incoming_and_outgoing program locs =
    let transitions =
      Set.to_sequence locs
      |> Sequence.map ~f:(fun loc ->
             let incoming = Sequence.of_list (ProbabilisticTransitionGraph.pred_e (graph program) loc) in
             let outgoing = Sequence.of_list (ProbabilisticTransitionGraph.succ_e (graph program) loc) in
             Sequence.append incoming outgoing)
      |> Sequence.join
    in
    Sequence.map ~f:ProbabilisticTransition.gt transitions |> GeneralTransitionSet.of_sequence


  let to_formatted_string ?(pretty = false) t =
    let transitions =
      Set.to_list (gts t)
      |> List.map
           ~f:
             (FormattedString.mk_str_line
             %
             if pretty then
               GeneralTransition.to_string_pretty
             else
               GeneralTransition.to_string)
      |> FormattedString.mappend
    in
    let locations = String.concat ~sep:", " @@ List.map ~f:Location.to_string @@ Set.to_list @@ locations t in
    FormattedString.format_append
      ([
         "Start:  " ^ Location.to_string (start t);
         "Program_Vars:  "
         ^ (input_vars t |> VarSet.map_to_list (Var.to_string ~pretty) |> String.concat ~sep:", ");
         "Temp_Vars:  " ^ (tmp_vars t |> VarSet.map_to_list (Var.to_string ~pretty) |> String.concat ~sep:", ");
         "Locations:  " ^ locations;
         "Transitions:";
       ]
      |> List.map ~f:FormattedString.mk_str_line
      |> FormattedString.mappend)
      transitions


  let to_string = FormattedString.render_string % to_formatted_string
  let to_string_pretty = FormattedString.render_string % to_formatted_string ~pretty:true

  let remove_zero_prob_transitions tset program =
    Set.fold tset ~init:program ~f:(fun prog (l, t, l') ->
        assert (RationalLaurentPolynomial.(equal (ProbabilisticTransitionLabel.probability t) zero));
        remove_transition prog (l, t, l'))
end

module ProbabilisticTransitionGraphNonProbOverappr = struct
  include TransitionGraph_.Make_ (ProbabilisticTransitionNonProbOverappr) (TransitionGraph_Ocamlgraph_Repr_)

  (** This is the same for normal prob. programs and non prob. overapproximated ones *)
  let add_invariant = ProbabilisticTransitionGraph.add_invariant
end

module ProbabilisticProgramNonProbOverappr = struct
  include
    Program_.Make (ProbabilisticTransitionLabelNonProbOverappr) (ProbabilisticTransitionNonProbOverappr)
      (ProbabilisticTransitionGraphNonProbOverappr)

  let add_invariant = ProbabilisticProgram.add_invariant
  let simplify_all_guards = ProbabilisticProgram.simplify_all_guards
  let remove_unsatisfiable_transitions = ProbabilisticProgram.remove_unsatisfiable_transitions
end

module GRV = struct
  module Trans = struct
    type t = GeneralTransition.t * Location.t

    let compare (gt1, l1) (gt2, l2) =
      match GeneralTransition.compare gt1 gt2 with
      | 0 -> Location.compare l1 l2
      | r -> r


    let sexp_of_t = Sexplib0.Sexp_conv.sexp_of_opaque
  end

  module TransComp = Comparator.Make (Trans)

  type transition = Trans.t
  type transition_comparator_witness = TransComp.comparator_witness
  type t = transition * Var.t

  type comparator_witness =
    (transition_comparator_witness, Var.comparator_witness) RVComparator.comparator_witness

  let comparator = RVComparator.comparator TransComp.comparator Var.comparator
  let compare = Comparator.compare_of_comparator comparator
  let equal = Comparator.equal_of_comparator comparator
  let sexp_of_t = Sexplib0.Sexp_conv.sexp_of_opaque
  let hash ((gt, l), v) = Hashtbl.hash (GeneralTransition.gt_id gt, Location.to_string l, Var.to_string v)
  let variable (_, v) = v
  let transition (t, _) = t
  let gt ((gt, _), _) = gt

  let to_id_string ((gt, l), v) =
    "|(" ^ GeneralTransition.to_id_string gt ^ "," ^ Location.to_string l ^ ")," ^ Var.to_string v ^ "|"


  let ids_to_string ?(pretty = false) ((gt, l), v) =
    "("
    ^ GeneralTransition.ids_to_string ~pretty gt
    ^ "," ^ Location.to_string l ^ "), " ^ Var.to_string ~pretty v


  let to_probabilistic_rvs ((gt, loc), v) =
    GeneralTransition.transitions gt |> Set.to_sequence
    |> Sequence.filter ~f:(Location.equal loc % ProbabilisticTransition.target)
    |> Sequence.map ~f:(fun t -> (t, v))
end

module RVTuple_ = struct
  type transition = ProbabilisticTransition.t
  type transition_comparator_witness = ProbabilisticTransition.comparator_witness
  type t = transition * Var.t

  let sexp_of_t = Sexplib0.Sexp_conv.sexp_of_opaque
  let hash (t1, v1) = Hashtbl.hash (ProbabilisticTransition.id t1, Var.to_string v1)
  let transition (t, _) = t
  let variable (_, v) = v

  type comparator_witness =
    (ProbabilisticTransition.comparator_witness, Var.comparator_witness) RVComparator.comparator_witness

  let comparator = RVComparator.comparator ProbabilisticTransition.comparator Var.comparator
  let compare = Comparator.compare_of_comparator comparator
  let equal = Comparator.equal_of_comparator comparator
end

module ProbabilisticRV = struct
  include RVTuple_

  let to_id_string (t, v) = "|" ^ ProbabilisticTransition.to_id_string t ^ "," ^ Var.to_string v ^ "|"

  let ids_to_string ?(pretty = false) (t, v) =
    ProbabilisticTransitionLabel.ids_to_string ~pretty (ProbabilisticTransition.label t)
    ^ ", " ^ Var.to_string ~pretty v
end

module ProbabilisticRVNonProbOverappr = struct
  include RVTuple_

  let to_id_string (t, v) =
    "|" ^ ProbabilisticTransitionNonProbOverappr.to_id_string t ^ "," ^ Var.to_string v ^ "|"


  let ids_to_string ?(pretty = false) (t, v) =
    ProbabilisticTransitionLabelNonProbOverappr.ids_to_string ~pretty
      (ProbabilisticTransitionNonProbOverappr.label t)
    ^ ", " ^ Var.to_string ~pretty v
end

module Equalities = struct
  let trans_eq : (ProbabilisticTransitionNonProbOverappr.t, ProbabilisticTransition.t) Type_equal.t =
    Type_equal.refl


  let rvtuple__eq : (ProbabilisticRVNonProbOverappr.t, ProbabilisticRV.t) Type_equal.t = Type_equal.refl

  let trans_cmp_wit_eq :
      ( ProbabilisticTransitionNonProbOverappr.comparator_witness,
        ProbabilisticTransition.comparator_witness )
      Type_equal.t =
    Type_equal.refl


  let rvtuple__cmp_wit_eq :
      (ProbabilisticRVNonProbOverappr.comparator_witness, ProbabilisticRV.comparator_witness) Type_equal.t =
    Type_equal.refl


  let program_equalities : (ProbabilisticProgram.t, ProbabilisticProgramNonProbOverappr.t) Type_equal.t =
    Type_equal.refl
end
