open OurBase
open Formulas
open Polynomials

exception ProbabilitiesDoNotSumToOne

(** This is a low level internal interface *)
module ProbabilisticTransitionLabel_ = struct
  module Inner =  struct
    module Invariant = Guard

    type t = {
      id : int;
      gt_id: int;
      probability: OurFloat.t;

      guard: Guard.t;
      invariant: Invariant.t;
      update: UpdateElement_.t ProgramTypes.var_map;

      (* overapproximated probabilistic updates for classical analysis *)
      overappr_nonprob_update: Polynomial.t ProgramTypes.var_map;
      overappr_guard: Guard.t;

      cost: Polynomial.t;
    }
    let sexp_of_t = Sexplib0.Sexp_conv.sexp_of_opaque

    let default = {
      id = 0;
      gt_id = 0;
      probability = OurFloat.one;

      guard = Guard.mk_true;
      invariant = Guard.mk_true;
      overappr_guard = Guard.mk_true; (* This guard goes along with overappr_nonprob_update *)

      update = Map.empty (module Var);
      overappr_nonprob_update = Map.empty (module Var);

      cost = Polynomial.one
    }

    let compute_overapproximated_update_and_guard update_map =
      Map.map ~f:(fun ue ->
          match UpdateElement_.to_polynomial ue with
          | Some p -> Guard.mk_true, p
          | None -> let v' = Var.fresh_id Var.Int () in UpdateElement_.as_guard ue v',Polynomial.of_var v'
        ) update_map
      |> fun m -> Guard.all (List.map ~f:Tuple2.first @@ Map.data m), Map.map ~f:Tuple2.second m

    let add_self_updates_if_missing to_update_value vars update_map =
      Set.fold ~f:(fun fold_update var ->
          if Map.mem update_map var then fold_update
          else Map.add_exn fold_update ~key:var ~data:(to_update_value var)
        ) vars ~init:update_map

    let equal t1 t2 = t1.id = t2.id

    let fill_up_update_arg_vars_up_to_num from_var n update =
      let all_args = (VarSet.of_sequence @@ Sequence.take Var.args n) in
      add_self_updates_if_missing from_var all_args update

    let fill_up_arg_vars_up_to_num n t =
      {t with update = fill_up_update_arg_vars_up_to_num UpdateElement_.of_var n t.update;
              overappr_nonprob_update = fill_up_update_arg_vars_up_to_num Polynomial.of_var n t.overappr_nonprob_update }


    let mk ~gt_id ~patterns ~probability ~assignments ~cost ~guard  =
      let map_to_arg_vars =
        Sequence.zip (Sequence.of_list patterns) Var.args
        |> RenameMap.of_sequence
      in
      let update =
        Sequence.of_list assignments
        |> Sequence.map ~f:(UpdateElement_.rename map_to_arg_vars)
        |> Sequence.zip Var.args
        |> Map.of_sequence_exn (module Var)
        |> fill_up_update_arg_vars_up_to_num UpdateElement_.of_var (List.length patterns)
      in

      let guard = Guard.rename guard map_to_arg_vars in
      let (overappr_guard, overappr_nonprob_update) = compute_overapproximated_update_and_guard update in
      let overappr_guard = Guard.mk_and overappr_guard guard in
      {
        id = Unique.unique ();
        gt_id;
        probability;

        guard;
        invariant = Guard.mk_true;
        overappr_guard;

        update;
        overappr_nonprob_update;

        cost = Polynomial.rename map_to_arg_vars cost;
      }

    let compare t1 t2 = Int.compare t1.id t2.id

    let id t = t.id
    let gt_id t = t.gt_id
    let probability t = t.probability
    let invariant t = t.invariant
    let cost t = t.cost
    let map_guard f t = { t with guard = f t.guard; overappr_guard = f t.overappr_guard; }

    let add_invariant t inv = { t with invariant = Guard.mk_and t.invariant inv }

    (** Returns a string representing the left hand side of the update function. Parameter {i to_file} is used to get a representation with less special characters. *)
    let update_to_string_lhs_ ~pretty t =
      let update = t.update in
      if Map.is_empty update then ""
      else
        Map.to_alist update
        |> List.map ~f:(fun (var,_) -> Var.to_string ~pretty var)
        |> fun xs -> "("^(String.concat ~sep:"," xs)^")"

    let update_to_string_lhs = update_to_string_lhs_ ~pretty:false
    let update_to_string_lhs_pretty = update_to_string_lhs_ ~pretty:true

    let update_to_string_rhs_ ue_to_string get_update t =
      let update = get_update t in
      if Map.is_empty update then ""
      else
        Map.to_alist update
        |> List.map ~f:(fun (_,poly) -> ue_to_string poly)
        |> fun xs -> "("^(String.concat ~sep:"," xs)^")"

    let update_to_string_rhs ue_to_string get_update = update_to_string_rhs_ ue_to_string get_update
    let update_to_string_rhs_pretty ue_to_string_pretty get_update = update_to_string_rhs_ ue_to_string_pretty get_update

    let update_to_string ue_to_string update =
      if Map.is_empty update then "true"
      else
        Map.to_alist update
        |> List.map ~f:(fun (var,poly) -> (Var.to_string var, ue_to_string poly))
        |> List.unzip
        |> fun (xs,ys) -> "("^(String.concat ~sep:"," xs)^") -> ("^(String.concat ~sep:"," ys)^")"

    let to_string ue_to_string ue_to_string_pretty get_update get_guard ?(pretty = false) t =
      let guard = if Guard.is_true (get_guard t)  then "" else " :|: " ^ Guard.to_string ~pretty (get_guard t) in
      let invariant = if Invariant.is_true t.invariant  then "" else " [" ^ Guard.to_string ~pretty t.invariant ^ "] " in
      let cost = if Polynomial.is_one t.cost then if pretty then "->" else "" else if pretty then "-{" ^ Polynomial.to_string_pretty t.cost else Polynomial.to_string t.cost ^ "}>" in
      if pretty then
        OurFloat.to_string t.probability ^ ":" ^ "t" ^ Util.natural_to_subscript t.id ^ "∈g" ^ Util.natural_to_subscript t.gt_id
        ^ ":" ^ invariant ^ " " ^ update_to_string_lhs_pretty t ^ " " ^ cost ^ " "
        ^ update_to_string_rhs_pretty ue_to_string_pretty get_update t ^ guard
      else
        "ID: " ^ OurFloat.to_string t.probability ^ ":" ^ invariant ^ string_of_int t.gt_id ^ "," ^ string_of_int t.id ^ ", " ^ cost ^ "&euro;" ^ ", " ^ update_to_string ue_to_string (get_update t) ^ guard

    let cost_to_string t =
      if Polynomial.is_one t.cost then ""
      else "{"^(Polynomial.to_string t.cost)^"}"

    let to_id_string t =
      string_of_int t.gt_id ^ "," ^ string_of_int t.id

    let input_vars t =
      VarSet.of_list @@ Map.keys t.update

    let input_size = Set.length % input_vars

    type vars_type = VarsOverapproximated
                   | VarsNonOverapproximated [@@deriving ord,sexp]

    let vars vars_type t =
      let update_and_guard_vars = match vars_type with
        | VarsOverapproximated ->
          Set.union
            (Map.fold ~f:(fun ~key ~data -> Set.union (Polynomial.vars data)) t.overappr_nonprob_update ~init:VarSet.empty)
            (Guard.vars t.overappr_guard)
        | VarsNonOverapproximated ->
          Set.union
            (Map.fold ~f:(fun ~key ~data -> Set.union (UpdateElement_.vars data)) t.update ~init:VarSet.empty)
            (Guard.vars t.guard)
      in
      update_and_guard_vars
      |> Set.union (Guard.vars t.invariant)
      |> Set.union (Polynomial.vars t.cost)
      |> Set.union (VarSet.of_list @@ Map.keys t.update)

    module VarsForMemoization = struct
      type t = vars_type * int [@@deriving ord,sexp]
      let hash = Hashtbl.hash
    end
    (* TODO May invalidate through invariant generation! *)
    let vars_memoization: (vars_type*int,VarSet.t) Hashtbl.t = Hashtbl.create ~size:10 (module VarsForMemoization)
    let clear_memoized_vars_for_id id =
      Hashtbl.remove vars_memoization (VarsNonOverapproximated,id);
      Hashtbl.remove vars_memoization (VarsOverapproximated,id)

    let rename_update rename_ue update rename_map =
      update
      |> Map.to_sequence
      |> Sequence.map ~f:(fun (key, value) -> RenameMap.find key rename_map ~default:key, rename_ue rename_map value)
      |> Map.of_sequence_exn (module Var)

    let rename rename_map t =
      { t with
        update                  = rename_update UpdateElement_.rename t.update                  rename_map;
        overappr_nonprob_update = rename_update Polynomial.rename    t.overappr_nonprob_update rename_map;

        guard          = Guard.rename t.guard          rename_map;
        overappr_guard = Guard.rename t.overappr_guard rename_map;
        invariant      = Guard.rename t.invariant      rename_map;

        cost = Polynomial.rename rename_map t.cost;
      }
      |> tap (fun _ -> clear_memoized_vars_for_id t.id)

    let rename_temp_vars t temp_vars =
      (* whenever we have a temp variable with the same name occur in both the overapproximated and the
        non overapproximated vars they refer to the same thing. *)
      let all_temp_vars =
        Set.diff
          (Set.union (vars VarsOverapproximated t) (vars VarsNonOverapproximated t))
          (input_vars t)
      in
      let rename_map =
        Sequence.zip (Set.to_sequence all_temp_vars) temp_vars
        |> RenameMap.of_sequence
      in
      rename rename_map t

    let remove_non_contributors non_contributors t =
      let update_ = Set.fold ~f:(fun u var -> Map.remove u var) non_contributors ~init:t.update in
      { t with
        update = update_;
      }
      |> tap (fun _ -> clear_memoized_vars_for_id t.id)

    let update_admissibility_constraint t: Guard.t =
      Map.to_sequence t.update
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
                  nondet_var
                )
          )
      in
      Guard.Infix.(t1.overappr_guard && t1.invariant
                  && Guard.map_polynomial (Polynomial.substitute_f (substitution t1.overappr_nonprob_update))
                                          t2.overappr_guard
                  && Guard.map_polynomial (Polynomial.substitute_f (substitution t1.overappr_nonprob_update))
                                          t2.invariant)

    let overapprox_nonlinear_updates t =
      let handle_nonlinear_or_probabilistic ue =
        let v' = Var.fresh_id Var.Int () in (Polynomial.of_var v', UpdateElement_.as_linear_guard t.guard ue v')
      in
      let overappr_nonprob_update, overappr_guard =
        Map.map ~f:(fun ue ->
            match UpdateElement_.to_polynomial ue with
            | Some p when Polynomial.degree p = 1 -> p, Guard.mk_true
            | _ -> handle_nonlinear_or_probabilistic ue
          ) t.update
        |> fun m -> Map.map ~f:Tuple2.first m, Guard.all (List.map ~f:Tuple2.second @@ Map.data m)
      in
      let overappr_guard = Guard.mk_and overappr_guard (Guard.drop_nonlinear t.overappr_guard) in
      { t with overappr_nonprob_update; overappr_guard; }

    let restore_legacy_distribution_update_semantics t =
      let update = Map.mapi ~f:(fun ~key ~data -> UpdateElement_.restore_legacy_distribution_update_semantics key data) t.update in
      let (overappr_guard, overappr_nonprob_update) =
        Map.map ~f:(fun ue ->
            match UpdateElement_.to_polynomial ue with
            | Some p -> Guard.mk_true, p
            | None -> let v' = Var.fresh_id Var.Int () in UpdateElement_.as_guard ue v',Polynomial.of_var v'
          ) update
        |> fun m -> Guard.all (List.map ~f:Tuple2.first @@ Map.data m), Map.map ~f:Tuple2.second m
      in
      { t with update; overappr_guard; overappr_nonprob_update; }
  end

  include Inner
  include Comparator.Make(Inner)
end

module ProbabilisticTransitionLabel = struct
  include ProbabilisticTransitionLabel_

  type update_element = UpdateElement_.t

  let update_map t = t.update
  let update t v = Map.find t.update v
  let guard t = Guard.mk_and t.invariant t.guard
  let guard_without_inv t = t.guard

  (** Returns if the two labels describe the same transition *)
  let equivalent t1 t2 =
    Map.equal UpdateElement_.equal t1.update t2.update
    && OurFloat.equal t1.probability t2.probability
    && Guard.equal t1.guard t2.guard
    && Invariant.equal t1.invariant t2.invariant
    && Polynomial.equal t1.cost t2.cost

  let compare_equivalent t1 t2 =
    if OurFloat.compare t1.probability t2.probability != 0 then
      OurFloat.compare t1.probability t2.probability
    else if Map.compare_direct UpdateElement_.compare t1.update t2.update != 0 then
      Map.compare_direct UpdateElement_.compare t1.update t2.update
    else if Guard.compare t1.guard t2.guard != 0 then
      Guard.compare t1.guard t2.guard
    else if Invariant.compare t1.invariant t2.invariant != 0 then
      Invariant.compare t1.invariant t2.invariant
    else if Polynomial.compare t1.cost t2.cost != 0 then
      Polynomial.compare t1.cost t2.cost
    else
      0

  let equivalent_update t1 t2 =
    Map.equal UpdateElement_.equal t1.update t2.update

  let ids_to_string ?(pretty=false) t =
    if pretty then
      "t"^Util.natural_to_subscript (id t)^" &isin; g"^Util.natural_to_subscript (gt_id t)
    else
      "t"^Int.to_string (id t)^" in g"^Int.to_string (gt_id t)

  let update_to_string_rhs = update_to_string_rhs UpdateElement_.to_string (fun t -> t.update)
  let update_to_string_rhs_pretty = update_to_string_rhs_pretty UpdateElement_.to_string_pretty (fun t -> t.update)
  let to_string =
    to_string
      UpdateElement_.to_string UpdateElement_.to_string_pretty
      (fun t -> t.update) (fun t -> t.guard)

  let vars = vars VarsNonOverapproximated
  let has_tmp_vars t = not @@ Set.is_empty @@ Set.diff (vars t) (input_vars t)

  let tmp_vars t =
    Set.diff (vars t) (input_vars t)

  let relax_guard ~non_static t =
    let is_static atom = Set.is_subset (Atoms.Atom.vars atom) ~of_:(Set.diff (input_vars t) non_static) in
    {t with guard = List.filter ~f:is_static t.guard;
            overappr_guard = List.filter ~f:is_static t.overappr_guard}

  let changed_vars t =
    input_vars t
    |> Set.filter ~f:(fun v -> not UpdateElement_.(equal (of_var v) (update t v |? of_var v)))

  let negative_costs t = SMT.Z3Solver.satisfiable Formula.(mk_and (mk @@ guard t) (mk_gt Polynomial.zero t.cost))
end

module ProbabilisticTransitionLabelNonProbOverappr = struct
  include ProbabilisticTransitionLabel_

  type update_element = Polynomial.t

  let update_map t = t.overappr_nonprob_update
  let update t v = Map.find t.overappr_nonprob_update v
  let guard t = Guard.mk_and t.invariant t.overappr_guard
  let guard_without_inv t = t.overappr_guard

  (** Returns if the two labels describe the same transition *)
  let equivalent t1 t2 =
    Map.equal Polynomial.equal t1.overappr_nonprob_update t2.overappr_nonprob_update
    && Guard.equal t1.overappr_guard t2.overappr_guard
    && Invariant.equal t1.invariant t2.invariant
    && Polynomial.equal t1.cost t2.cost

  let compare_equivalent t1 t2 =
    if Map.compare_direct Polynomial.compare t1.overappr_nonprob_update t2.overappr_nonprob_update != 0 then
      Map.compare_direct Polynomial.compare t1.overappr_nonprob_update t2.overappr_nonprob_update
    else if Guard.compare t1.overappr_guard t2.overappr_guard != 0 then
      Guard.compare t1.overappr_guard t2.overappr_guard
    else if Invariant.compare t1.invariant t2.invariant != 0 then
      Invariant.compare t1.invariant t2.invariant
    else if Polynomial.compare t1.cost t2.cost != 0 then
      Polynomial.compare t1.cost t2.cost
    else
      0

  let equivalent_update t1 t2 =
    Map.equal Polynomial.equal t1.overappr_nonprob_update t2.overappr_nonprob_update

  let ids_to_string ?(pretty=false) t =
    if pretty then "t" ^ Util.natural_to_subscript (id t)
    else "t" ^ Int.to_string (id t)

  let update_to_string_rhs = update_to_string_rhs Polynomial.to_string (fun t -> t.overappr_nonprob_update)
  let update_to_string_rhs_pretty = update_to_string_rhs_pretty Polynomial.to_string_pretty (fun t -> t.overappr_nonprob_update)
  let to_string =
    to_string
      Polynomial.to_string Polynomial.to_string_pretty
      (fun t -> t.overappr_nonprob_update) (fun t -> t.overappr_guard)

  let vars = vars VarsOverapproximated

  let tmp_vars t = Set.diff (vars t) (input_vars t)

  let relax_guard ~non_static t =
    let is_static atom = Set.is_subset (Atoms.Atom.vars atom) ~of_:(Set.diff (input_vars t) non_static) in
    {t with guard = List.filter ~f:is_static t.guard;
            overappr_guard = List.filter ~f:is_static t.overappr_guard}

  let has_tmp_vars t = not @@ Set.is_empty @@ Set.diff (vars t) (input_vars t)
  let negative_costs t = SMT.Z3Solver.satisfiable Formula.(mk_and (mk @@ guard t) (mk_gt Polynomial.zero t.cost))

  let changed_vars t =
    input_vars t
    |> Set.filter ~f:(fun v -> not Polynomial.(equal (of_var v) (update t v |? of_var v)))
end

(** Shared definitions between ProbabilisticTransition and ProbabilisticTransitionNonProbOverAppr *)
module ProbabilisticTransitionShared = struct
  module Inner = struct
    (* do not use include here so that we have to manually check changes in TransitionOver *)
    module GenericTransition = Transition_.TransitionOver(ProbabilisticTransitionLabel)(Location)

    type location = GenericTransition.location
    type location_comparator_witness = Location.comparator_witness
    type t = GenericTransition.t
    type transition_label = GenericTransition.transition_label
    type transition_label_comparator_witness = GenericTransition.transition_label_comparator_witness

    let src = GenericTransition.src
    let label = GenericTransition.label
    let target = GenericTransition.target
    let id = GenericTransition.id
    let gt_id (_,label,_) = ProbabilisticTransitionLabel.gt_id label
    let compare = GenericTransition.compare
    let equal = GenericTransition.equal
    let equivalent = GenericTransition.equivalent
    let compare_equivalent = GenericTransition.compare_equivalent
    let hash = GenericTransition.hash
    let overapprox_nonlinear_updates = GenericTransition.overapprox_nonlinear_updates
    let map_label = GenericTransition.map_label
    let add_invariant = GenericTransition.add_invariant
    let cost = GenericTransition.cost
    let rename = GenericTransition.rename
  end

  include Inner

  type comparator_witness = (transition_label_comparator_witness,location_comparator_witness) ProgramTypes.TransitionComparator.comparator_witness
  let comparator = ProgramTypes.TransitionComparator.comparator ProbabilisticTransitionLabel.comparator Location.comparator
  let sexp_of_t = Sexplib0.Sexp_conv.sexp_of_opaque
end

module ProbabilisticTransition = struct
  include ProbabilisticTransitionShared

  let vars (_,label,_) = ProbabilisticTransitionLabel.vars label
  let input_vars (_,label,_) = ProbabilisticTransitionLabel.input_vars label

  let to_string (l,t,l') =
    Int.to_string (ProbabilisticTransitionLabel.gt_id t)^":"
    ^ Int.to_string (ProbabilisticTransitionLabel.id t)^":"
    ^ Location.to_string l ^ ProbabilisticTransitionLabel.update_to_string_lhs t^ " -"
    ^ ProbabilisticTransitionLabel.cost_to_string t^"> "
    ^ Location.to_string l'
    ^ ProbabilisticTransitionLabel.update_to_string_rhs t
    ^ if Constraints.Constraint.is_true (ProbabilisticTransitionLabel.guard t) then ""
      else ":|:" ^ Guard.to_string (ProbabilisticTransitionLabel.guard t)

  let to_string_pretty (l,t,l') =
    "t" ^ Util.natural_to_subscript (ProbabilisticTransitionLabel.id t)
    ^"∈g"^ Util.natural_to_subscript (ProbabilisticTransitionLabel.gt_id t) ^": "
    ^ Location.to_string l ^ ProbabilisticTransitionLabel.update_to_string_lhs_pretty t
    ^ (if Polynomials.Polynomial.(equal one (ProbabilisticTransitionLabel.cost t)) then " → "
       else " -"^ ProbabilisticTransitionLabel.cost_to_string t^"> " )
    ^ Location.to_string l'
    ^ ProbabilisticTransitionLabel.update_to_string_rhs_pretty t
    ^ if Constraints.Constraint.is_true (ProbabilisticTransitionLabel.guard t) then ""
      else " :|: " ^ Guard.to_string ~pretty:true (ProbabilisticTransitionLabel.guard t)


  let to_id_string (l,label,l') =
    Int.to_string (ProbabilisticTransitionLabel.gt_id label)^":"
    ^ Int.to_string (ProbabilisticTransitionLabel.id label) ^ ": " ^ Location.to_string l ^ "->"
    ^ OurFloat.to_string (ProbabilisticTransitionLabel.probability label)^ ":"^ Location.to_string l'

  let to_id_string_pretty  (l,label,l') =
    "t" ^ Util.natural_to_subscript (ProbabilisticTransitionLabel.id label)
    ^ "∈g" ^ Util.natural_to_subscript (ProbabilisticTransitionLabel.gt_id label) ^ ": " ^ Location.to_string l ^ "→"
    ^ OurFloat.to_string (ProbabilisticTransitionLabel.probability label) ^ ":"^ Location.to_string l'

  let to_string_rhs (_,label,l') =
    let prob = ProbabilisticTransitionLabel.probability label in
    let prob_string =
      if OurFloat.(equal one prob) then "" else "[" ^ OurFloat.to_string prob ^ "]:"
    in
    prob_string ^ (Int.to_string (ProbabilisticTransitionLabel.id label) ^ ":") ^ Location.to_string l' ^ ProbabilisticTransitionLabel.update_to_string_rhs label

  let to_string_rhs_pretty (_,label,l') =
    let prob = ProbabilisticTransitionLabel.probability label in
    let prob_string = if OurFloat.(equal one prob) then "" else "["^OurFloat.to_string prob ^ "]:" in
    prob_string ^ ("t"^Util.natural_to_subscript (ProbabilisticTransitionLabel.id label) ^ ":") ^ Location.to_string l' ^ ProbabilisticTransitionLabel.update_to_string_rhs_pretty label

  let restore_legacy_distribution_update_semantics (l,label,l') =
    l,ProbabilisticTransitionLabel.restore_legacy_distribution_update_semantics label,l'

  let same_gt t1 t2 = Int.equal (gt_id t1) (gt_id t2)
end

module ProbabilisticTransitionNonProbOverappr = struct
  include ProbabilisticTransitionShared

  let to_id_string = GenericTransition.to_id_string
  let to_id_string_pretty = GenericTransition.to_id_string_pretty
  let to_string = GenericTransition.to_string
  let to_string_pretty = GenericTransition.to_string_pretty
end

module GeneralTransition = struct
  module ProbabilisticTransitionSet = Transition_.TransitionSetOver(ProbabilisticTransition)(Location)

  module Inner = struct
    type t = {
      transitions: ProbabilisticTransitionSet.t;
    }

    let sexp_of_t = Sexplib0.Sexp_conv.sexp_of_opaque

    let get_arbitrary_transition t = Set.choose_exn t.transitions
    let get_arbitrary_label = ProbabilisticTransition.label % get_arbitrary_transition

    let guard = ProbabilisticTransitionLabel.guard % get_arbitrary_label

    let map_transitions f t =
      { transitions = ProbabilisticTransitionSet.map ~f t.transitions }

    let mk ~(start:Location.t)
          ~(fill_up_to_num_arg_vars:int)
          ~(patterns:Var.t list)
          ~(cost:Polynomials.Polynomial.t)
          ~(guard:Guard.t)
          ~(rhss: (OurFloat.t * UpdateElement_.t list * Location.t) list) : t =
      let total_prob =
        Sequence.of_list rhss
        |> Sequence.map ~f:Tuple3.first
        |> OurFloat.sum
      in
      (if not (OurFloat.equal OurFloat.one total_prob) then raise ProbabilitiesDoNotSumToOne);

      let gt_id = Unique.unique () in
      let rhss =
        List.map
          ~f:(fun(p,ues,l) -> ProbabilisticTransitionLabel.fill_up_arg_vars_up_to_num fill_up_to_num_arg_vars @@
            ProbabilisticTransitionLabel_.mk ~gt_id ~patterns ~probability:p ~assignments:ues ~cost ~guard, l)
          rhss
      in

      (* add update_admissibility constraints *)
      let update_admissibility =
        Sequence.map ~f:Tuple2.first (Sequence.of_list rhss)
        |> Sequence.map ~f:ProbabilisticTransitionLabel.update_admissibility_constraint
        |> Sequence.fold ~f:Guard.mk_and ~init:Guard.mk_true
      in
      let rhss =
        List.map ~f:(Tuple2.map1 (ProbabilisticTransitionLabel_.map_guard (Guard.mk_and update_admissibility))) rhss
      in

      (* rename program vars to arg vars *)
      let rename_map = RenameMap.of_sequence @@ Sequence.zip (Sequence.of_list patterns) Var.args in
      let rhss = List.map ~f:(Tuple2.map1 (ProbabilisticTransitionLabel_.rename rename_map)) rhss in

      { transitions = ProbabilisticTransitionSet.of_list @@
          List.map ~f:(fun (label,target) -> start,label,target) rhss; }

    let gt_id = ProbabilisticTransitionLabel.gt_id % get_arbitrary_label
    let transitions t = t.transitions
    let cost = ProbabilisticTransitionLabel.cost % get_arbitrary_label

    let src = ProbabilisticTransition.src % get_arbitrary_transition
    let invariant = ProbabilisticTransitionLabel.invariant % get_arbitrary_label
    let guard_without_inv = ProbabilisticTransitionLabel.guard_without_inv % get_arbitrary_label
    let targets t =
      (Set.of_sequence (module Location) @@ Sequence.map ~f:ProbabilisticTransition.target (Set.to_sequence t.transitions))

    let equal t1 t2 = Int.equal (gt_id t1) (gt_id t2)
    let compare t1 t2 = Int.compare (gt_id t1) (gt_id t2)
    let hash = Hashtbl.hash % gt_id

    let add_invariant gt inv =
      { transitions = ProbabilisticTransitionSet.map ~f:(ProbabilisticTransition.add_invariant inv) gt.transitions }

    let vars t =
      Set.to_sequence t.transitions
      |> Sequence.map ~f:ProbabilisticTransition.vars
      |> Sequence.fold ~f:Set.union ~init:VarSet.empty

    let input_vars t =
      Set.to_sequence t.transitions
      |> Sequence.map ~f:ProbabilisticTransition.input_vars
      |> Sequence.fold ~f:Set.union ~init:VarSet.empty

    let locations t = Set.add (targets t) (src t)

    let to_id_string t =
      let show_prob_branch (_,label,l') =
        Int.to_string (ProbabilisticTransitionLabel.id label) ^ ": " ^ OurFloat.to_string (ProbabilisticTransitionLabel.probability label) ^ ":" ^ Location.to_string l'
      in
      Int.to_string (gt_id t) ^ ":" ^ Location.to_string (src t) ^ "->" ^ Util.sequence_to_string ~f:show_prob_branch (Set.to_sequence t.transitions)

    let to_id_string_pretty t =
      let show_prob_branch (_,label,l') =
        "t"^Util.natural_to_subscript (ProbabilisticTransitionLabel.id label) ^ ":" ^
        OurFloat.to_string (ProbabilisticTransitionLabel.probability label) ^ ":" ^ Location.to_string l'
      in
      "g"^Util.natural_to_subscript (gt_id t) ^ ":" ^ Location.to_string (src t) ^ "→" ^ Util.sequence_to_string ~f:show_prob_branch (Set.to_sequence t.transitions)

    let to_string t =
      let id_string = Int.to_string (gt_id t) ^": " in
      let vars = ProbabilisticTransitionLabel.update_to_string_lhs @@ ProbabilisticTransition.label (Set.choose_exn t.transitions) in
      let trans = Set.to_sequence t.transitions in
      let cost_str = if Polynomial.(equal (cost t) one) then "" else "{" ^ Polynomial.to_string (cost t) ^ "}" in
      let trans_string =
        Sequence.map ~f:ProbabilisticTransition.to_string_rhs trans
        |> Sequence.reduce ~f:(fun a b -> a^" :+: "^b) |? ""
      in
      id_string ^ Location.to_string (src t) ^ vars ^ " -" ^ cost_str ^ "> " ^ trans_string ^ " :|: " ^ Guard.to_string ~pretty:false (guard t)

    let to_string_pretty t =
      let id_string = "g" ^ Util.natural_to_subscript (gt_id t) ^ ":" in
      let vars = ProbabilisticTransitionLabel.update_to_string_lhs_pretty @@ ProbabilisticTransition.label (Set.choose_exn t.transitions) in
      let trans = Set.to_sequence t.transitions in
      let cost_str = if Polynomial.(equal (cost t) one) then "" else "{" ^ Polynomial.to_string_pretty (cost t) ^ "}" in
      let trans_string =
        Sequence.map ~f:ProbabilisticTransition.to_string_rhs_pretty trans
        |> Sequence.reduce ~f:(fun a b -> a^" :+: "^b) |? ""
      in
      let arrow = if String.is_empty cost_str then "→"  else "-"^cost_str^">" in
      id_string ^ Location.to_string (src t) ^ vars ^ " " ^ arrow ^ " " ^ trans_string ^ " :|: " ^ Guard.to_string ~pretty:true (guard t)

    let remove_non_contributors non_contributors gt =
      let transitions' =
        ProbabilisticTransitionSet.map ~f:(fun (l,label,l') ->
            let update' = Set.fold ~f:Map.remove non_contributors ~init:label.update in
            (l,{ label with update = update' },l')
          )
          gt.transitions
      in
      { transitions = transitions' }

    let ids_to_string ?(pretty=false) gt =
      if pretty then "g" ^ Util.natural_to_subscript (gt_id gt)
      else "g" ^ Int.to_string (gt_id gt)
  end

  include Inner
  include Comparator.Make(Inner)
end

module GeneralTransitionSet = struct
  module LocationSet = Location.LocationSetOver(Location)
  module GeneralTransition = GeneralTransition

  type location = Location.t
  type location_comparator_witness = Location.comparator_witness
  type location_set = LocationSet.t


  include MakeSetCreators0(GeneralTransition)

  let to_string =
    Util.sequence_to_string ~f:GeneralTransition.to_id_string % Set.to_sequence

  let to_id_string = Util.sequence_to_string ~f:GeneralTransition.to_id_string % Set.to_sequence

  let locations t =
    Set.fold ~f:(fun set gt ->
        Set.add set (GeneralTransition.src gt)
        |> Set.union (GeneralTransition.targets gt)
      )
      t ~init:(Set.empty (module Location))

  let targets gts =
    Set.to_sequence  gts
    |> Sequence.map ~f:GeneralTransition.targets
    |> Sequence.fold ~f:(Set.union) ~init:(Set.empty (module Location))

  let all_transitions t =
    Sequence.map ~f:(Set.to_sequence % GeneralTransition.transitions) (Set.to_sequence t)
    |> Set.of_sequence (module ProbabilisticTransition) % Sequence.join
end

(* Probabilistic and Overapproximated Nonprobabilistic Programs share the same internal *)
(* TransitionGraph representation to allow for an efficient conversion between both *)
(* Here we use the internal ProbabilisticTransitionLabel_ *)
module TransitionGraph_Ocamlgraph_Repr_ =
  Graph.Persistent.Digraph.ConcreteBidirectionalLabeled(Location)
    (ProbabilisticTransitionLabel_)


module ProbabilisticTransitionGraph =
  TransitionGraph_.Make_(ProbabilisticTransition)(Location)(TransitionGraph_Ocamlgraph_Repr_)

module ProbabilisticProgram = struct
  include Program_.Make(ProbabilisticTransitionLabel)(ProbabilisticTransition)(Location)(ProbabilisticTransitionGraph)

  module ProbabilisticTransitionSet = Transition_.TransitionSetOver(ProbabilisticTransition)(Location)

  let gts t =
    let arr = Set.to_array (transitions t) in
    Array.sort ~compare:ProbabilisticTransition.compare arr;
    Array.to_sequence arr
    |> Sequence.group ~break:(fun t1 t2 -> ProbabilisticTransition.gt_id t1 <> ProbabilisticTransition.gt_id t2)
    |> Sequence.map ~f:(fun l -> { GeneralTransition.transitions = ProbabilisticTransitionSet.of_list l })
    |> GeneralTransitionSet.of_sequence

  let pre_gt t gt =
    (* All transitions in a gt have the same guard and hence the same pre transitions *)
    let pre_t =
      pre t (Set.choose_exn @@ GeneralTransition.transitions gt)
    in
    Set.filter
      ~f:(Set.exists ~f:(Set.mem pre_t) % GeneralTransition.transitions)
      (gts t)

  let from_gts (start: Location.t) gts =
    Set.to_sequence (GeneralTransitionSet.all_transitions gts)
    |> from_sequence start

  let restore_legacy_distribution_update_semantics =
    map_transitions ProbabilisticTransition.restore_legacy_distribution_update_semantics

  let tmp_vars t =
    Set.diff (vars t) (input_vars t)

  let is_initial_gt t gt =
    Location.equal (start t) (GeneralTransition.src gt)

  let sccs_gts t =
    let gts = gts t in
    sccs t
    |> List.map ~f:(fun tset ->
        Set.filter ~f:(fun gt ->
            let gt_transs = GeneralTransition.transitions gt in
            Set.exists ~f:(fun t -> Set.mem gt_transs t) tset
          ) gts
      )

  let to_formatted_string ?(pretty=false) t =
    let transitions =
      Set.to_list (gts t)
      |> List.map ~f:(FormattedString.mk_str_line
                     % if pretty then GeneralTransition.to_string_pretty else GeneralTransition.to_string)
      |> FormattedString.mappend
    in
    let locations = String.concat ~sep:", " @@ List.map ~f:Location.to_string @@ Set.to_list @@ locations t in
    FormattedString.format_append (
      [ "Start:  "^Location.to_string (start t);
        "Program_Vars:  "^(input_vars t |> VarSet.map_to_list (Var.to_string ~pretty) |> String.concat ~sep:", ");
        "Temp_Vars:  "^(tmp_vars t |> VarSet.map_to_list (Var.to_string ~pretty) |> String.concat ~sep:", ");
        "Locations:  "^locations;
        "Transitions:";]
    |> List.map ~f:(FormattedString.mk_str_line) |> FormattedString.mappend) transitions

  let to_string = FormattedString.render_string % to_formatted_string
  let to_string_pretty = FormattedString.render_string % to_formatted_string ~pretty:true
end


module ProbabilisticTransitionGraphNonProbOverappr =
  TransitionGraph_.Make_(ProbabilisticTransitionNonProbOverappr)(Location)(TransitionGraph_Ocamlgraph_Repr_)

module ProbabilisticProgramNonProbOverappr = struct
  include Program_.Make(ProbabilisticTransitionLabelNonProbOverappr)(ProbabilisticTransitionNonProbOverappr)
      (Location)(ProbabilisticTransitionGraphNonProbOverappr)
end


let compare_rv compare_transition (t1,v1) (t2,v2) =
  if compare_transition t1 t2 != 0 then
    compare_transition t1 t2
  else if Var.compare v1 v2 != 0 then
    Var.compare v1 v2
  else
    0

module GRV = struct
  module RVTuple_ = struct
    module Inner = struct
      type transition = GeneralTransition.t * Location.t
      type t = transition * Var.t
      let sexp_of_t = Sexplib0.Sexp_conv.sexp_of_opaque
      let equal ((gt1,l1),v1) ((gt2,l2),v2) =
        GeneralTransition.equal gt1 gt2
        && Location.equal l1 l2
        && Var.equal v1 v2
      let hash ((gt,l),v) = Hashtbl.hash (GeneralTransition.gt_id gt, Location.to_string l, Var.to_string v)
      let compare =
        let cmp (gt1,l1) (gt2,l2) =
          match GeneralTransition.compare gt1 gt2 with
          | 0 -> Location.compare l1 l2
          | r -> r
        in
        compare_rv cmp
    end

    include Inner
    include Comparator.Make(Inner)
  end

  type transition = RVTuple_.transition
  type t = RVTuple_.t
  let equal = RVTuple_.equal
  let hash = RVTuple_.hash
  let compare = RVTuple_.compare

  let to_id_string ((gt,l),v) =
    "|(" ^ GeneralTransition.to_id_string gt ^ "," ^ Location.to_string l ^ ")," ^ Var.to_string v ^ "|"

  let ids_to_string ?(pretty=false) ((gt,l),v) =
    "("^ GeneralTransition.ids_to_string ~pretty gt ^ "," ^ Location.to_string l
    ^ "), " ^ Var.to_string ~pretty v
end

module RVTuple_ = struct
  module Inner = struct
    type transition = ProbabilisticTransition.t
    type t = transition * Var.t
    let sexp_of_t = Sexplib0.Sexp_conv.sexp_of_opaque
    let equal (t1,v1) (t2,v2) = ProbabilisticTransition.equal t1 t2 && Var.equal v1 v2
    let hash (t1,v1) = Hashtbl.hash (ProbabilisticTransition.id t1, Var.to_string v1)
    let compare = compare_rv ProbabilisticTransition.compare
  end

  include Inner
  include Comparator.Make(Inner)
end
module RVTupleNonProbOverappr_ = RVTuple_

module ProbabilisticRV = struct
  module RVTuple_ = RVTuple_

  type transition = RVTuple_.transition
  type t = RVTuple_.t
  let equal = RVTuple_.equal
  let hash = RVTuple_.hash
  let compare = compare_rv ProbabilisticTransition.compare

  let to_id_string (t,v) =
    "|" ^ ProbabilisticTransition.to_id_string t ^ "," ^ Var.to_string v ^ "|"

  let ids_to_string ?(pretty=false) (t,v) =
    ProbabilisticTransitionLabel.ids_to_string ~pretty (ProbabilisticTransition.label t) ^ ", " ^ Var.to_string ~pretty v
end
module ProbabilisticRVNonProbOverappr = struct
  module RVTuple_ = RVTupleNonProbOverappr_

  type transition = RVTupleNonProbOverappr_.transition
  type t = RVTupleNonProbOverappr_.t
  let equal = RVTupleNonProbOverappr_.equal
  let hash = RVTupleNonProbOverappr_.hash
  let compare = compare_rv ProbabilisticTransitionNonProbOverappr.compare

  let to_id_string (t,v) =
    "|" ^ ProbabilisticTransitionNonProbOverappr.to_id_string t ^ "," ^ Var.to_string v ^ "|"

  let ids_to_string ?(pretty=false) (t,v) =
    ProbabilisticTransitionLabelNonProbOverappr.ids_to_string ~pretty (ProbabilisticTransitionNonProbOverappr.label t)
    ^ ", " ^ Var.to_string ~pretty v
end

module Equalities = struct
  let trans_eq: (ProbabilisticTransitionNonProbOverappr.t,ProbabilisticTransition.t) Type_equal.t =
    Type_equal.refl
  let rvtuple__eq: (ProbabilisticRVNonProbOverappr.RVTuple_.t, ProbabilisticRV.RVTuple_.t) Type_equal.t =
    Type_equal.refl
  let trans_cmp_wit_eq: (ProbabilisticTransitionNonProbOverappr.comparator_witness, ProbabilisticTransition.comparator_witness) Type_equal.t =
    Type_equal.refl
  let rvtuple__cmp_wit_eq:
    ( ProbabilisticRVNonProbOverappr.RVTuple_.comparator_witness
    , ProbabilisticRV.RVTuple_.comparator_witness) Type_equal.t = Type_equal.refl
end