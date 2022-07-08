(** Updates size and time-bounds until a fixed point is reached. *)
open Batteries
open ProgramTypes
open Polynomials
open LocalSizeBound

let find_bounds ?(mprf_max_depth = 1) ~preprocess ~local ~cfr ?(time_cfr = 180) ?(twn = false) (program: Program.t) (appr: Approximation.t): Program.t * Approximation.t =
  print_string "bounds 8 \n";
  let t = Big_int.big_int_of_int 5  in 
  let ads : Var.t = Var.of_string "x" in 
  let y = Polynomials.Polynomial.of_coeff_list [t; t;t] [ads; Var.of_string "x"; Var.of_string "y"]  in 
  print_string (Polynomials.Polynomial.to_string y);
  (*print_string program;*)
  print_bool twn;
  Printf.printf("\n");
  (*print_string appr;*)
  print_int time_cfr;
  let rvg_with_sccs = RVGTypes.RVG.rvg_with_sccs program in
   if not (List.is_empty cfr) then
    PartialEvaluation.time_cfr := float_of_int time_cfr;
  let (program_cfr,updated_appr) = appr
  |> TrivialTimeBounds.compute program
  |> Analysis.improve rvg_with_sccs ~mprf_max_depth ~preprocess ~local ~cfr `Time program in
  let appr_cost =
  updated_appr
  |> (fun appr ->
    if program_cfr |> Program.transitions |> TransitionSet.exists (fun t -> not (Polynomial.is_const (Transition.cost t))) then
      Tuple2.second @@ Analysis.improve rvg_with_sccs ~mprf_max_depth ~preprocess ~local ~cfr `Cost program_cfr appr
    else
      appr
  )
  |> CostBounds.infer_from_timebounds program_cfr in
  (program_cfr,appr_cost)
