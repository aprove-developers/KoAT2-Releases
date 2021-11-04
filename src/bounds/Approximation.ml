open Batteries
open BoundsInst
open ProgramTypes
open ApproximationModules
open Formatter
open FormattedString

type t = {
    time: TransitionApproximation.t;
    size: SizeApproximation.t;
    cost: TransitionApproximation.t;
  }

let equivalent appr1 appr2 =
  TransitionApproximation.equivalent appr1.time appr2.time
  && SizeApproximation.equivalent appr1.size appr2.size

let empty transitioncount varcount = {
    time = TransitionApproximation.empty "time" transitioncount;
    size = SizeApproximation.empty (2 * transitioncount * varcount);
    cost = TransitionApproximation.empty "cost" transitioncount;
  }

let create program =
  empty (TransitionGraph.nb_edges (Program.graph program))
        (VarSet.cardinal (Program.vars program))

let time appr = appr.time

let size appr = appr.size

let cost appr = appr.cost


(** Sizebound related methods *)

let sizebound =
  SizeApproximation.get % size

let add_sizebound bound transition var appr =
  { appr with size = SizeApproximation.add bound transition var appr.size }

let add_sizebounds bound scc appr =
  { appr with size = SizeApproximation.add_all bound scc appr.size }

(** Timebound related methods *)

let timebound =
  TransitionApproximation.get % time

let timebound_id =
  TransitionApproximation.get_id % time

let program_timebound =
  TransitionApproximation.sum % time

let add_timebound bound transition appr =
  { appr with time = TransitionApproximation.add bound transition appr.time }

let all_times_bounded =
  TransitionApproximation.all_bounded % time

let is_time_bounded appr =
  not % Bound.is_infinity % timebound appr

(** Costbound related methods *)

let costbound =
  TransitionApproximation.get % cost

let program_costbound =
  TransitionApproximation.sum % cost

let add_costbound bound transition appr =
  { appr with cost = TransitionApproximation.add bound transition appr.cost }

let min program appr1 appr2 = 
  create program
  |> TransitionSet.fold (
    fun t appr -> 
    let min_timebound = Bound.keep_simpler_bound (timebound appr1 t) (timebound appr2 t) in
    let min_costbound = Bound.keep_simpler_bound (costbound appr1 t) (costbound appr2 t) in
    VarSet.fold (fun var appr ->
      let min_sizebound = Bound.keep_simpler_bound (sizebound appr1 t var) (sizebound appr2 t var) in appr |> add_sizebound min_sizebound t var) 
      (Program.input_vars program)
      appr |> add_timebound min_timebound t |> add_costbound min_costbound t
  ) (Program.transitions program)

let to_formatted ?(show_initial=false) ?(pretty=false) program appr =
  let overall_timebound = program_timebound appr program in
  mk_str_header_big "All Bounds" <>
  if show_initial then
    mk_paragraph (
     (mk_str_header_small "Initial Complexity Problem (after preprocessing)"
        <> (Program.to_formatted_string program) <> mk_newline
     ) )
  else FormattedString.Empty

  <> mk_str_header_small "Timebounds" <> ( mk_paragraph (
       mk_str_line ("Overall timebound:" ^ Bound.to_string ~pretty overall_timebound)
       <> TransitionApproximation.to_formatted ~pretty (Program.transitions program |> TransitionSet.to_list) appr.time) )

  <> mk_str_header_small "Costbounds" <> ( mk_paragraph (
        mk_str_line ("Overall costbound: " ^ Bound.to_string ~pretty (program_costbound appr program))
        <> TransitionApproximation.to_formatted ~pretty (Program.transitions program |> TransitionSet.to_list) appr.cost ) )

  <> mk_str_header_small "Sizebounds" <> (mk_paragraph @@ SizeApproximation.to_formatted ~pretty appr.size)


(* TODO: use to_formatted *)
let to_string program appr =
  let overall_costbound = program_costbound appr program in
  let output = IO.output_string () in
    if (not (Bound.is_infinity overall_costbound)) then
      IO.nwrite output ("YES( ?, " ^ Bound.to_string (overall_costbound) ^ ")\n\n")
    else
      IO.nwrite output "MAYBE\n\n";
    IO.nwrite output "Initial Complexity Problem After Preprocessing:\n";
    IO.nwrite output (Program.to_string program^"\n");
    IO.nwrite output "Timebounds: \n";
    IO.nwrite output ("  Overall timebound: " ^ Bound.to_string (program_timebound appr program) ^ "\n");
    appr.time |> TransitionApproximation.to_string (TransitionSet.to_list @@ Program.transitions program) |> IO.nwrite output;
    IO.nwrite output "\nCostbounds:\n";
    IO.nwrite output ("  Overall costbound: " ^ Bound.to_string (overall_costbound) ^ "\n");
    appr.cost |> TransitionApproximation.to_string (TransitionSet.to_list @@ Program.transitions program) |> IO.nwrite output;
    IO.nwrite output "\nSizebounds:\n";
    appr.size |> SizeApproximation.to_string |> IO.nwrite output;
    IO.close_out output
