open! OurBase
open ProbabilisticProgramModules

(* TODO unify both. Should be possible if we always keep the target location *)
let entry_gts_with_locs program of_gts =
  Set.to_sequence of_gts
  |> Sequence.map ~f:(fun gt ->
         let loc = GeneralTransition.src gt in
         Set.to_sequence (Program.pre_gt program gt)
         |> Sequence.filter ~f:(fun gt -> not (Set.mem of_gts gt))
         |> Sequence.map ~f:(fun gt -> (gt, loc)))
  |> Sequence.join


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

  let substitute_bound_with_exp_and_class_sizes_and_get_size ~exp_subst ~class_subst b =
    match B.to_poly b with
    | None -> (B.substitute_f class_subst b, class_subst)
    | Some p -> (
        let all_vars = Poly.vars p in
        let vars_of_degree_one =
          Poly.monomials_with_coeffs p
          |> List.fold_left ~init:all_vars ~f:(fun vset (_, m) ->
                 let nonlinear_vars =
                   Mon.to_sequence m
                   |> Sequence.filter_map ~f:(fun (v, e) -> Option.some_if (e <> 1) v)
                   |> VarSet.of_sequence
                 in
                 Set.diff vset nonlinear_vars)
          |> Set.to_list
        in
        let best_exp_subst =
          let all_possibilities =
            let open ListMonad in
            let+ v_exp = vars_of_degree_one in
            ( v_exp,
              B.substitute_f
                (fun v ->
                  if Var.equal v v_exp then
                    exp_subst v
                  else
                    class_subst v)
                b )
          in
          all_possibilities |> List.min_elt ~compare:(fun (_, b1) (_, b2) -> B.compare_asy b1 b2)
        in
        match best_exp_subst with
        | Some (v_exp, b) ->
            ( b,
              fun v ->
                if Var.equal v v_exp then
                  exp_subst v_exp
                else
                  class_subst v )
        | None -> (B.substitute_f class_subst b, class_subst))


  let substitute_bound_with_exp_and_class_sizes ~exp_subst ~class_subst b =
    let b, _ = substitute_bound_with_exp_and_class_sizes_and_get_size ~exp_subst ~class_subst b in
    b


  let substitute_bound_with_exp_and_class_sizes_get_size ~exp_subst ~class_subst b =
    let _, get_size = substitute_bound_with_exp_and_class_sizes_and_get_size ~exp_subst ~class_subst b in
    get_size
end

module SubstHelper = MakeSubstHelper (OurInt)
module RationalSubstHelper = MakeSubstHelper (OurRational)
