(** Updates size and time-bounds until a fixed point is reached. *)
open Batteries
open ProgramTypes
open Polynomials
open LocalSizeBound

(** Updates size and time-bounds until a fixed point is reached and uses, if necessary, MRFs or CFR. *)
(* let rec find_bounds_ ?(mrf = false) ?(cfr = false) (program: Program.t) (appr: Approximation.t): Program.t * Approximation.t =
  let (updated_appr, program_cfr) = appr
  |> SizeBounds.improve program
  |> RankingBounds.improve  ~mrf:mrf ~cfr:cfr `Time program 
  in 
    if MaybeChanged.has_changed program_cfr then 
        let unpacked_program_cfr = MaybeChanged.unpack program_cfr in
          LocalSizeBound.reset ();
          unpacked_program_cfr
          |> Approximation.create
          |> TrivialTimeBounds.compute unpacked_program_cfr 
          |> SizeBounds.improve unpacked_program_cfr 
          |> find_bounds_ ~mrf:mrf ~cfr:cfr unpacked_program_cfr
    else if MaybeChanged.has_changed updated_appr then
      updated_appr
      |> MaybeChanged.unpack
      |> find_bounds_  ~mrf:mrf ~cfr:cfr program
    else 
      (program,appr) *)

(** Triggers size and time-bounds computations and, if necessary, sets maximal depth of MRFs. *)
(* let find_bounds ?(depth = 5) ?(mrf = false) ?(cfr = false) (program: Program.t) (appr: Approximation.t): Program.t * Approximation.t =
  if mrf then( 
    MultiphaseRankingFunction.maxDepth := depth;
    MultiphaseRankingFunction.list_init depth);
  let (program_cfr,updated_appr) = appr
  |> TrivialTimeBounds.compute program
  |> find_bounds_ ~mrf:mrf ~cfr:cfr program in
  let appr_cost =
  updated_appr
  |> (fun appr ->
    if program_cfr |> Program.transitions |> TransitionSet.exists (fun t -> not (Polynomial.is_one (Transition.cost t))) then
      RankingBounds.improve `Cost program_cfr appr
      |> fst
    else
      MaybeChanged.same appr
  )
  |> MaybeChanged.unpack
  |> CostBounds.infer_from_timebounds program_cfr in
  (program_cfr,appr_cost) *)

  let find_bounds ?(depth = 5) ?(mrf = false) ?(cfr = false) ?(time_cfr = 180) (program: Program.t) (appr: Approximation.t): Program.t * Approximation.t =
    if cfr then
      CFR.number_unsolved_trans := (Program.cardinal_trans_scc program);
      CFR.time_cfr := float_of_int time_cfr;
    if mrf then( 
      MultiphaseRankingFunction.maxDepth := depth;
      MultiphaseRankingFunction.list_init depth);
  let (program_cfr,updated_appr) = appr
  |> TrivialTimeBounds.compute program
  |> RankingBounds.improve ~mrf:mrf ~cfr:cfr `Time program in
  let appr_cost =
  updated_appr
  |> (fun appr ->
    if program_cfr |> Program.transitions |> TransitionSet.exists (fun t -> not (Polynomial.is_one (Transition.cost t))) then
      RankingBounds.improve ~mrf:mrf ~cfr:false `Cost program_cfr appr
      |> snd
    else
      appr
  )
  |> CostBounds.infer_from_timebounds program_cfr in
  (program_cfr,appr_cost)