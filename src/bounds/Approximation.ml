open Batteries
open BoundsInst
open ProgramTypes
open ApproximationModules
open Formatter
open FormatMonad.Monad
open FormatMonad

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

let program_expsizebound appr program var =
  Program.generalized_transitions program
  |> GeneralTransitionSet.enum
  |> Enum.map (fun gt ->
      GeneralTransition.targets gt
      |> LocationSet.enum
      |> Enum.map (fun l -> expsizebound `Upper appr (gt,l) var)
     )
  |> Enum.flatten
  |> RealBound.sum

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
  { appr with time = TransitionApproximation.add bound transition appr.time }

let add_timebound_gt bound gt appr =
  { appr with time_gt = GeneralTransitionNonProbApproximation.add bound gt appr.time_gt }

let add_exptimebound simplify_smt bound gt appr =
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

(* use the cost bounds as overall if no other bounds are provided *)
let overall_result_string ?(overall_expbound=None) program expected appr =
    if expected then
      let overall_expcostbound =
        if Option.is_some overall_expbound then Option.get
          overall_expbound
        else
          program_expcostbound appr program
      in

      if (RealBound.is_infinity overall_expcostbound) then
        "MAYBE"
      else
        "WORST_CASE( ?, " ^ RealBound.to_string (overall_expcostbound) ^ ")"

    else
      let overall_costbound = program_costbound appr program in
      if Bound.is_infinity overall_costbound then
        "MAYBE"
      else
        "WORST_CASE( ?, " ^ Bound.to_string overall_costbound ^ ")"

let output_formatted ?(embeddings=[]) program expected appr =
  let overall_timebound = program_timebound appr program in
  let overall_exptimebound = program_exptimebound appr program in

  paragraph (
   (str_header_big "Initial Complexity Problem (after preprocessing)"
      >> (write_format @@ Program.to_formatted_string program) >> newline
      (* only include graph in html output *)
      >> mapM write_format embeddings ) )

  >> paragraph
      (str_header_big "Timebounds: "
        >> str_line ("  Overall timebound:" ^ Bound.to_string overall_timebound)
        >> write_format (TransitionApproximation.to_formatted (Program.transitions program |> TransitionSet.to_list) appr.time))

  (* Additionally write expected timebounds if requested *)
  >> when_m expected
       (paragraph (
          str_header_big "Expected Timebounds:"
          >> str_line ("  Overall expected timebound: " ^ RealBound.to_string overall_exptimebound)
          >> write_format
            (GeneralTransitionApproximation.to_formatted
              (Program.generalized_transitions program |> GeneralTransitionSet.to_list) appr.exptime) ))

  >> paragraph
      (str_header_big "Costbounds:"
        >> str_line ("  Overall costbound: " ^ Bound.to_string (program_costbound appr program))
        >> write_format (TransitionApproximation.to_formatted (Program.transitions program |> TransitionSet.to_list) appr.cost) )

  >> when_m expected
      (paragraph (
        str_header_big "Expected Costbounds:"
        >> str_line ("  Overall expected costbound: " ^ RealBound.to_string (program_expcostbound appr program))
        >> write_format
          (GeneralTransitionApproximation.to_formatted
            (Program.generalized_transitions program |> GeneralTransitionSet.to_list) appr.expcost)))

  >> paragraph(
      str_header_big "Sizebounds:"
      >> write_format (SizeApproximation.to_formatted appr.size))

  >> when_m expected
      (paragraph (
        str_header_big "ExpSizeBounds:"
        >> write_format (ExpectedSizeApproximation.to_formatted ~print_lower:false appr.expsize)))

let output prog exp appr =
  render_default ~format:Plain @@ output_formatted ~embeddings:[] prog exp appr
