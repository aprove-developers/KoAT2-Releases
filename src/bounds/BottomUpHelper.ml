open Batteries
open ProgramTypes
open BoundsInst

let logger = Logging.(get BottomUp)

let bottom_up_candidates program appr =
  let execute () =
    let sccs =
      let arr =
        Program.all_sccs program
        |> Array.of_enum
      in
      Array.sort (fun s1 s2 -> compare (TransitionSet.cardinal s1) (TransitionSet.cardinal s2)) arr;
      Array.enum arr
    in
    let all_gts = Program.generalized_transitions program in
    let containing_gts scc =
      scc
      |> TransitionSet.enum
      |> Enum.map (fun t -> GeneralTransitionSet.any @@ GeneralTransitionSet.filter (TransitionSet.mem t % GeneralTransition.transitions) all_gts)
      |> GeneralTransitionSet.of_enum
    in
    let gts_set_all_trans gtsset =
      GeneralTransitionSet.enum gtsset
      |> Enum.map GeneralTransition.transitions
      |> Enum.fold TransitionSet.union TransitionSet.empty
    in
    sccs
    (* Check which sccs have 'closed' general transitions *)
    |> Enum.map (fun scc -> scc,containing_gts scc)
    |> Enum.filter (fun (scc,containing_gts) -> TransitionSet.equal scc (gts_set_all_trans containing_gts))
    (* Now check for all sccs which have a missing expected costbound *)
    |> Enum.map Tuple2.second
    |> Enum.filter @@ GeneralTransitionSet.exists (RealBound.is_infinity % Approximation.expcostbound appr)
  in
  Logger.with_log
    logger Logger.DEBUG
    (fun () -> "bottom_up_candidates", []) ~result:(Util.enum_to_string (GeneralTransitionSet.to_id_string) % Enum.clone)
    execute

let create_sub_program trans_id_counter scc scc_locs program: Program.t =
  let execute () =
    let new_start_location = Location.of_string @@ Printf.sprintf "loc_start_bu_%i" (Batteries.unique ()) in

    let modified_outgoing_of_scc: TransitionSet.t =
      Program.transitions program
      |> TransitionSet.filter (fun t -> (LocationSet.mem (Transition.src t) scc_locs) && (not @@ LocationSet.mem (Transition.target t) scc_locs))
      |> TransitionSet.map (Transition.update_cost (Polynomials.Polynomial.zero, RealBound.zero))
    in
    let new_start_transitions =
      Program.transitions program
      |> TransitionSet.enum
      (* get incoming transitions *)
      |> Enum.filter (fun t -> (not @@ LocationSet.mem (Transition.src t) scc_locs) && (LocationSet.mem (Transition.target t) scc_locs))
      |> Enum.map Transition.target
      |> Enum.uniq_by (Location.equal)
      |> Enum.filter ((flip LocationSet.mem) scc_locs)
      |> Enum.map
        (
          fun l' ->
            let new_label =
              TransitionLabel.make_prob
                trans_id_counter
                ~cvect:(Polynomials.Polynomial.zero, RealBound.zero)
                ~input_vars_ordered:(Program.input_vars program |> VarSet.to_list)
                ~update:(
                  Program.input_vars program
                  |> VarSet.enum
                  |> Enum.map (fun v -> (v, TransitionLabel.UpdateElement.Poly (Polynomials.Polynomial.of_var v)))
                  |> TransitionLabel.VarMap.of_enum
                )
                ~guard:(TransitionLabel.Guard.mk_true)
                ~gt_id:(TransitionLabel.get_unique_gt_id trans_id_counter ())
                ~probability:(OurFloat.one)
                "Com_1"
            in
            (new_start_location,new_label,l')
        )
      (*  Append original outgoing transitions to obtain sizebounds *)
      |> TransitionSet.of_enum
    in
    let all_new_transitions = TransitionSet.union modified_outgoing_of_scc new_start_transitions in

    Program.from
      (
        TransitionSet.union
          (* scc transitions *)
          (GeneralTransitionSet.enum scc |> Enum.map GeneralTransition.transitions |> Enum.fold TransitionSet.union TransitionSet.empty)
          (all_new_transitions)
        |> TransitionSet.to_list
      )
      new_start_location
  in
  Logger.with_log logger Logger.DEBUG (fun () -> "create_sub_program", []) ~result:(Program.to_string ~show_gtcost:true) execute

let untouched_scc_vars scc program =
  let execute = fun () ->
    let vars = Program.input_vars program in
    let transitions =
      GeneralTransitionSet.enum scc
      |> Enum.map GeneralTransition.transitions
      |> Enum.fold TransitionSet.union TransitionSet.empty
    in
    TransitionSet.fold
      (fun (l,t,l') vset ->
        TransitionLabel.VarMap.fold
          (fun v u vset ->
            match u with
            | TransitionLabel.UpdateElement.Poly p ->
                if Polynomials.Polynomial.(equal p (of_var v)) then vset else VarSet.remove v vset
            | TransitionLabel.UpdateElement.Dist d -> VarSet.remove v vset)
          (TransitionLabel.update_map t)
          vset
      )
      transitions
      vars
  in
  Logger.with_log
    logger
    Logger.DEBUG
    (fun () -> "untouched_scc_vars", ["scc", GeneralTransitionSet.to_id_string scc])
    ~result:(VarSet.to_string) execute

(* Cuts the given SCC out of the program and inserts a newly created general transition which is then returned
   together with the modified program *)
let cut_scc trans_id_counter scc scc_locs program cvect:  (Program.t * GeneralTransition.t) =
    let new_location1 = Location.of_string @@ Printf.sprintf "loc_bu_%i" (Batteries.unique ()) in
    let new_location2 = Location.of_string @@ Printf.sprintf "loc_bu_%i" (Batteries.unique ()) in
    let untouched_vars = untouched_scc_vars scc program in
    let new_label =
        TransitionLabel.(make_prob
            trans_id_counter
            ~cvect:cvect
            ~input_vars_ordered:(Program.input_vars program |> VarSet.to_list)
            ~update:(
                VarSet.fold
                  (fun v ->
                    if VarSet.mem v untouched_vars then
                      VarMap.add v (UpdateElement.Poly (Polynomials.Polynomial.of_var v))
                    else
                      VarMap.add v (Var.fresh_id Var.Int () |> Polynomials.Polynomial.of_var |> fun p -> UpdateElement.Poly p)
                  )
                  (Program.vars program)
                  VarMap.empty
            )
            ~guard:(Guard.mk_true)
            ~gt_id:(get_unique_gt_id trans_id_counter ())
            ~probability:OurFloat.one
            "Com_1"
        )
    in
    let new_transition: Transition.t = (new_location1, new_label, new_location2) in
    let new_gt = GeneralTransition.of_transitionset (TransitionSet.singleton new_transition) new_transition in

    Program.add_locations ([new_location1; new_location2] |> List.enum) program
    |> Program.map_graph (Program.add_transitions (Enum.singleton new_transition))
    |> Program.map_graph
      (fun graph ->
        TransitionGraph.fold_edges_e
          (fun e graph' ->
            let (l,t,l') = e in
            match (LocationSet.mem l scc_locs, LocationSet.mem l' scc_locs) with
            | (true , true ) -> TransitionGraph.remove_edge_e graph' e
            | (false, true ) -> TransitionGraph.remove_edge_e graph' e |> (flip TransitionGraph.add_edge_e) (l,t,new_location1)
            | (true , false) -> TransitionGraph.remove_edge_e graph' e |> (flip TransitionGraph.add_edge_e) (new_location2,t,l')
            | (false, false) -> graph'
          )
          graph graph
      )
    |> LocationSet.fold (flip Program.remove_location) scc_locs
    |> fun prog -> (prog, new_gt)

let perform_step generate_invariants trans_id_counter program appr find_exp_bounds candidate_scc: (Program.t * Approximation.t) Option.t =
  let execute () =
    let scc_locs =
      GeneralTransitionSet.enum candidate_scc
      |> Enum.map (fun gt -> LocationSet.union (GeneralTransition.targets gt) (LocationSet.singleton (GeneralTransition.start gt)))
      |> Enum.fold LocationSet.union LocationSet.empty
    in

    let subprog = create_sub_program trans_id_counter candidate_scc scc_locs program in
    let sub_appr : Approximation.t =
      let sub_appr_start = Approximation.create subprog in
      Tuple2.second @@ find_exp_bounds (CacheManager.new_cache_with_counter trans_id_counter ()) subprog sub_appr_start
    in

    let sub_prog_incoming_nonprobabilistic k v: Bound.t =
      Program.transitions program
      |> TransitionSet.enum
      |> Enum.filter (fun (l,t,l') -> (not @@ LocationSet.mem l scc_locs) && (LocationSet.mem l' scc_locs) )
      |> Enum.map (fun t -> Approximation.sizebound k appr t v)
      |> match k with
         | `Lower -> Bound.minimum
         | `Upper -> Bound.maximum
    in

    (* We set the nonprobabilistic Cost to 0. Hence from now on we can not consider nonprobabilistic cost bounds*)
    let cvect =
      (Polynomials.Polynomial.zero, Approximation.program_expcostbound sub_appr subprog)
      |> tap (fun (_,c) -> Logger.log logger Logger.DEBUG (fun () -> "cvect new_gt",["expcost", RealBound.to_string c]))
     in
    let (new_prog,new_gt) =
      cut_scc trans_id_counter candidate_scc scc_locs program cvect
      |> Tuple2.map1 generate_invariants
    in

    let new_appr =
      (* Add all known Sizebounds/expsizebounds for the cut scc to the new general transition*)
      let all_outgoing_transitions =
        Program.transitions new_prog
        |> TransitionSet.filter (fun t -> (LocationSet.mem (Transition.src t) scc_locs, LocationSet.mem (Transition.target t) scc_locs) = (true, false))
      in
      Approximation.create new_prog
      |> TransitionSet.fold
          (fun t ->
            VarSet.fold
              (fun v ->
                List.fold_right
                  (fun k ->
                    Approximation.add_sizebound
                      k
                      (
                        Approximation.sizebound k sub_appr t v
                        |> Bound.appr_substitution k ~lower:(sub_prog_incoming_nonprobabilistic `Lower) ~higher:(sub_prog_incoming_nonprobabilistic `Upper)
                      )
                      t v
                  )
                  [`Upper; `Lower]
              )
              (Transition.label t |> TransitionLabel.vars)
          )
          all_outgoing_transitions
    in
    (* Return None if the subprogs cost is infinity as well because then nothing is won *)
    if RealBound.is_infinity (Tuple2.second cvect) then
      None
    else
      Some (new_prog, new_appr)
  in
  Logger.with_log
    logger Logger.DEBUG (fun () -> "perform_step", ["candidate_scc", GeneralTransitionSet.to_id_string candidate_scc])
    ~result:(fun o -> if Option.is_some o then "some" else "none")
    execute

let perform_bottom_up ~generate_invariants ~find_exp_bounds trans_id_counter program appr: (Program.t * Approximation.t) Option.t =
  bottom_up_candidates program appr
  |> Enum.map (perform_step generate_invariants trans_id_counter program appr find_exp_bounds)
  |> Enum.filter Option.is_some
  |> Enum.peek
  |> Util.flat_option
