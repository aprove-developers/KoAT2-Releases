open! OurBase
open ProbabilisticProgramModules
module GTAndLoc = MakeComparatorForTuples (GeneralTransition) (Location)

let entry_gts_with_locs program of_gts =
  Set.to_sequence of_gts
  |> Sequence.map ~f:(fun gt ->
         let loc = GeneralTransition.src gt in
         Set.to_sequence (Program.pre_gt program gt)
         |> Sequence.filter ~f:(fun gt -> not (Set.mem of_gts gt))
         |> Sequence.map ~f:(fun gt -> (gt, loc)))
  |> Sequence.join
  |> Set.of_sequence (module GTAndLoc)
  |> Set.to_sequence


let entry_gts program of_gts =
  Sequence.map ~f:Tuple2.first (entry_gts_with_locs program of_gts)
  |> Set.of_sequence (module GeneralTransition)


let entry_locations program of_gts =
  entry_gts_with_locs program of_gts |> Sequence.map ~f:Tuple2.second |> LocationSet.of_sequence


(** We use a functor to obtain methods for different bound types *)
module MakeSubstHelper (Num : PolyTypes.OurNumber) = struct
  module B = Bounds.Make (Num)
  module Poly = Polynomials.PolynomialOver (Num)
  module Mon = Monomials.Make (Num)

  (* This method could be improved by selecting a variable that is overapproximated using exp_subst per monomial.
      However, this would complicate the current design as TWN requires a "get_sizebound" function. *)
  let substitute_bound_with_exp_and_class_sizes_and_get_size ?proof ~exp_subst ~class_subst b =
    match B.to_poly_overappr_logs b with
    | None ->
        Option.iter proof ~f:(fun p ->
            ProofOutput.LocalProofOutput.add_to_proof p (fun () ->
                FormattedString.mk_str_line @@ "Substitute " ^ B.to_string ~pretty:true b
                ^ " with classical bounds only"));
        (B.substitute_f class_subst b, class_subst)
    | Some p ->
        let b = B.of_poly p in
        let all_vars = Poly.vars p in
        (* Vars that only occur in linear monomials can always be safely overapproximated*)
        let linear_in_vars =
          Poly.monomials_with_coeffs p
          |> List.fold_left ~init:all_vars ~f:(fun vset (_, m) ->
                 if Mon.is_univariate_linear m then
                   all_vars
                 else
                   Set.diff all_vars (Mon.vars m))
        in
        let vars_of_degree_one_but_poly_not_linear_in =
          let vars_of_degree_one =
            Poly.monomials_with_coeffs p
            |> List.fold_left ~init:all_vars ~f:(fun vset (_, m) ->
                   let nonlinear_vars =
                     Mon.to_sequence m
                     |> Sequence.filter_map ~f:(fun (v, e) -> Option.some_if (e <> 1) v)
                     |> VarSet.of_sequence
                   in
                   Set.diff vset nonlinear_vars)
          in
          Set.diff vars_of_degree_one linear_in_vars
        in
        let get_subst subst_with_exp v =
          if Set.mem subst_with_exp v then
            exp_subst v
          else
            class_subst v
        in
        let perform_subst subst_with_exp = B.substitute_f (get_subst subst_with_exp) b in
        (* We might choose one of the variables that occur with degree 1 to be overapproximated with exp_subst *)
        let best_exp_subst =
          let all_possibilities =
            let open ListMonad in
            let+ v_exp = Set.to_list vars_of_degree_one_but_poly_not_linear_in in
            let exp_vars = Set.add linear_in_vars v_exp in
            (exp_vars, perform_subst exp_vars)
          in
          all_possibilities |> List.min_elt ~compare:(fun (_, b1) (_, b2) -> B.compare_asy b1 b2)
        in
        let subst_with_exp, bound =
          match best_exp_subst with
          | Some (exp_vars, b) -> (exp_vars, b)
          | None -> (linear_in_vars, perform_subst linear_in_vars)
        in
        Option.iter proof ~f:(fun p ->
            ProofOutput.LocalProofOutput.add_to_proof p (fun () ->
                let subst_with_class = Set.diff all_vars subst_with_exp in
                FormattedString.mk_str_line @@ "Substitute " ^ B.to_string ~pretty:true b
                ^ " by classical bounds for: "
                ^ VarSet.to_string ~pretty:true subst_with_class
                ^ " and expected bounds for: "
                ^ VarSet.to_string ~pretty:true subst_with_exp));
        (bound, get_subst subst_with_exp)


  let substitute_bound_with_exp_and_class_sizes ?proof ~exp_subst ~class_subst b =
    let b, _ = substitute_bound_with_exp_and_class_sizes_and_get_size ?proof ~exp_subst ~class_subst b in
    b


  let substitute_bound_with_exp_and_class_sizes_get_size ?proof ~exp_subst ~class_subst b =
    let _, get_size =
      substitute_bound_with_exp_and_class_sizes_and_get_size ?proof ~exp_subst ~class_subst b
    in
    get_size
end

module SubstHelper = MakeSubstHelper (OurInt)
module RationalSubstHelper = MakeSubstHelper (OurRational)
