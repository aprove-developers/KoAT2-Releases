open Batteries
open BoundsInst
open ProgramTypes
open ApproximationModules

type kind = [ `Lower | `Upper ] [@@deriving eq, ord, show]

let logger = Logging.(get Approximation)

(* time_gt bounds the (non-probailistic) time for general transitions. It is more precise then the sum of time for all transitions in a general transition*)
type t = {
    time: TransitionApproximation.t;
    time_gt: GeneralTransitionNonProbApproximation.t;
    exptime: GeneralTransitionApproximation.t;
    size: SizeApproximation.t;
    expsize: ExpectedSizeApproximation.t;
    cost: TransitionApproximation.t;
    expcost: GeneralTransitionApproximation.t;
  }

let equivalent appr1 appr2 =
  TransitionApproximation.equivalent appr1.time appr2.time
  && SizeApproximation.equivalent appr1.size appr2.size

let empty transitioncount varcount gtcount = {
    time = TransitionApproximation.empty "time" transitioncount;
    time_gt = GeneralTransitionNonProbApproximation.empty "time" gtcount;
    exptime = GeneralTransitionApproximation.empty "exptime" gtcount;
    size = SizeApproximation.empty (2 * transitioncount * varcount);
    expsize = ExpectedSizeApproximation.empty (2 * gtcount * varcount);
    cost = TransitionApproximation.empty "cost" transitioncount;
    expcost = GeneralTransitionApproximation.empty "exptime" gtcount;
  }

let create program =
  empty (TransitionGraph.nb_edges (Program.graph program))
        (VarSet.cardinal (Program.vars program))
        (Program.generalized_transitions program |> GeneralTransitionSet.cardinal)

let time appr = appr.time

let time_gt appr = appr.time_gt

let exptime appr = appr.exptime

let size appr = appr.size

let expsize appr = appr.expsize

let cost appr = appr.cost

let expcost appr = appr.expcost

(** Sizebound related methods *)

let sizebound kind =
  SizeApproximation.get kind % size

let expsizebound kind =
  ExpectedSizeApproximation.get kind % expsize

let expsizebound_abs appr trans v =
  ExpectedSizeApproximation.get `Upper (expsize appr) trans v

let add_sizebound kind bound transition var appr =
  { appr with size = SizeApproximation.add kind bound transition var appr.size }

let add_expsizebound simplify_smt bound (gt,l) var appr =
  { appr with expsize =
      ExpectedSizeApproximation.add `Lower (RealBound.zero) (gt,l) var appr.expsize
      |> (fun appr -> if simplify_smt then
           ExpectedSizeApproximation.add ~simplifyfunc:(SimplifySMT.simplify_bound_with_smt_all_positive logger) `Upper bound (gt,l) var appr
         else
           ExpectedSizeApproximation.add ~simplifyfunc:RealBound.simplify_vars_nonnegative `Upper bound (gt,l) var appr )
         }

let add_sizebounds kind bound scc appr =
  { appr with size = SizeApproximation.add_all kind bound scc appr.size }

let add_expsizebounds simplify_smt bound scc appr =
  { appr with expsize =
      if simplify_smt then
        ExpectedSizeApproximation.add_all_abs ~simplifyfunc:(SimplifySMT.simplify_bound_with_smt_all_positive logger) bound scc appr.expsize
      else
        ExpectedSizeApproximation.add_all_abs ~simplifyfunc:RealBound.simplify_vars_nonnegative bound scc appr.expsize
  }

(** Timebound related methods *)

let timebound =
  TransitionApproximation.get % time

let timebound_gt =
  GeneralTransitionNonProbApproximation.get % time_gt

let exptimebound =
  GeneralTransitionApproximation.get % exptime

let timebound_id =
  TransitionApproximation.get_id % time

let program_timebound =
  TransitionApproximation.sum % time

let program_exptimebound =
  GeneralTransitionApproximation.sum % exptime

let add_timebound bound transition appr =
(*   let label = Transition.label transition in
  let temp_vars = VarSet.diff (TransitionLabel.vars label) (TransitionLabel.input_vars label) in
  let temp_bound = fun kind var -> if (VarSet.mem var temp_vars) then sizebound kind appr transition var else Bound.of_var var in
  let replaced_bound = Bound.appr_substitution `Upper ~lower:(temp_bound `Lower) ~higher:(temp_bound `Upper) bound in
  { appr with time = TransitionApproximation.add replaced_bound transition appr.time } *)
  { appr with time = TransitionApproximation.add bound transition appr.time }

let add_timebound_gt bound gt appr =
(*   (* TODO: Correct? Union on both sides? *)
  let temp_vars = VarSet.diff (GeneralTransition.vars gt) (GeneralTransition.input_vars gt) in
  let l = GeneralTransition.start gt in
  let temp_bound kind var = if (VarSet.mem var temp_vars) then expsizebound kind appr (gt,l) var else Bound.of_var var in
  let replaced_bound = Bound.appr_substition_abs_all (fun v -> Bound.abs_bound @@ fun k -> temp_bound k v) bound in
  { appr with time_gt = GeneralTransitionNonProbApproximation.add replaced_bound gt appr.exptime } *)
  { appr with time_gt = GeneralTransitionNonProbApproximation.add bound gt appr.time_gt }

let add_exptimebound simplify_smt bound gt appr =
(*   (* TODO: Correct? Union on both sides? *)
  let temp_vars = VarSet.diff (GeneralTransition.vars gt) (GeneralTransition.input_vars gt) in
  let l = GeneralTransition.start gt in
  let temp_bound kind var = if (VarSet.mem var temp_vars) then expsizebound kind appr (gt,l) var else RealBound.of_var var in
  let replaced_bound = RealBound.appr_substition_abs_all (fun v -> RealBound.abs_bound @@ fun k -> temp_bound k v) bound in
  { appr with exptime = GeneralTransitionApproximation.add replaced_bound gt appr.exptime } *)
  { appr with exptime =
      if simplify_smt then
        GeneralTransitionApproximation.add ~simplifyfunc:(SimplifySMT.simplify_bound_with_smt_all_positive logger) bound gt appr.exptime
      else
        GeneralTransitionApproximation.add ~simplifyfunc:RealBound.simplify_vars_nonnegative bound gt appr.exptime
  }


let all_times_bounded =
  TransitionApproximation.all_bounded % time

let is_time_bounded appr =
  not % Bound.is_infinity % timebound appr

let is_exptime_bounded appr =
  not % RealBound.is_infinity % exptimebound appr

(** Costbound related methods *)

let costbound =
  TransitionApproximation.get % cost

let expcostbound =
  GeneralTransitionApproximation.get % expcost

let is_expcost_bounded appr =
  not % RealBound.is_infinity % expcostbound appr

let program_costbound =
  TransitionApproximation.sum % cost

let program_expcostbound =
  GeneralTransitionApproximation.sum % expcost

let add_costbound bound transition appr =
  { appr with cost = TransitionApproximation.add bound transition appr.cost }

let add_expcostbound simplify_smt bound gt appr =
  { appr with expcost =
      if simplify_smt then
        GeneralTransitionApproximation.add ~simplifyfunc:(SimplifySMT.simplify_bound_with_smt_all_positive logger) bound gt appr.expcost
      else
        GeneralTransitionApproximation.add ~simplifyfunc:RealBound.simplify_vars_nonnegative bound gt appr.expcost
  }

let to_string ?(html=false) program expected appr=
  let html_header = ["<!DOCTYPE html>";"<html>";"<head>";"<title> KoAT2 Proof </title>";"</head>"] in
  let html_body_begin = ["<body>";"<p>";] in
  let html_body_end = ["</p>";"</body>";"</html>"] in
  let begin_head = if html then "<h3>" else "" in
  let end_head = if html then "</h3>" else "" in
  let endline = if html then "<br>\n" else "\n" in
  let overall_timebound = program_timebound appr program in
  let overall_exptimebound = program_exptimebound appr program in
  let output = IO.output_string () in
    if expected then
      if (not (RealBound.is_infinity overall_exptimebound)) then
        IO.nwrite output ("WORST_CASE( ?, " ^ RealBound.to_string (overall_exptimebound) ^ ")" ^ "\n")
      else
        IO.nwrite output ("MAYBE" ^ "\n")
    else
      if (not (Bound.is_infinity overall_timebound)) then
        IO.nwrite output ("WORST_CASE( ?, " ^ Bound.to_string (overall_timebound) ^ ")" ^ "\n")
      else
        IO.nwrite output ("MAYBE" ^ "\n");
    if html then
      IO.nwrite output (String.concat "\n" (html_header @ html_body_begin))
    else
      ();

    IO.nwrite output (begin_head ^ "Initial Complexity Problem:" ^ end_head ^ endline);
    IO.nwrite output (Program.to_string ~html:html ~show_gtcost:expected program ^ endline);
    if html then
      IO.nwrite output (GraphPrint.get_system_for_paper ~format:"svg" program); 
    IO.nwrite output (begin_head ^ "Timebounds:" ^ end_head ^ endline);
    IO.nwrite output ("  Overall timebound: " ^ Bound.to_string (overall_timebound) ^ endline);
    appr.time |> TransitionApproximation.to_string ~html:html  (Program.transitions program |> TransitionSet.to_list) |> IO.nwrite output;
    if expected then
      (IO.nwrite output (endline ^ begin_head ^ "Expected Timebounds: " ^ end_head ^ endline);
      IO.nwrite output ("  Overall expected timebound: " ^ RealBound.to_string (overall_exptimebound) ^ endline);
      appr.exptime
      |> GeneralTransitionApproximation.to_string ~html:html
          (Program.generalized_transitions program |> GeneralTransitionSet.to_list) |> IO.nwrite output);
    IO.nwrite output (endline ^ begin_head ^ "Costbounds:" ^ end_head ^ endline);
    IO.nwrite output ("  Overall costbound: " ^ Bound.to_string (program_costbound appr program) ^ endline);
    appr.cost |> TransitionApproximation.to_string ~html:html (Program.transitions program |> TransitionSet.to_list) |> IO.nwrite output;
    if expected then
      IO.nwrite output (endline ^ begin_head ^ "Expected Costbounds:" ^ end_head ^ endline);
      IO.nwrite output ("  Overall expected costbound: " ^ RealBound.to_string (program_expcostbound appr program) ^ endline);
      appr.expcost |> GeneralTransitionApproximation.to_string ~html:html (Program.generalized_transitions program |> GeneralTransitionSet.to_list) |> IO.nwrite output;
    IO.nwrite output (endline ^ begin_head ^ "Sizebounds:" ^ end_head ^ endline);
    appr.size |> SizeApproximation.to_string ~html:html |> IO.nwrite output;
    if expected then
      (IO.nwrite output (endline ^ begin_head ^ "ExpSizebounds:" ^ end_head ^ endline);
      appr.expsize |> ExpectedSizeApproximation.to_string ~html:html ~print_lower:false |> IO.nwrite output);
    if html then
      IO.nwrite output (String.concat "\n" (html_body_end));
    IO.close_out output;
