open Batteries
open BoundsInst
open ProgramModules
open Formatter
open FormattedString

module Make(B: BoundType.Bound)(PM: ProgramTypes.ProgramModules) = struct
  open PM
  module TransitionApproximation = TransitionApproximationType.Make(B)(PM)
  module SizeApproximation = SizeApproximationType.Make(B)(RV)

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

  let sizebound appr t v =
    SizeApproximation.get (size appr) (t,v)

  let add_sizebound bound transition var appr =
    { appr with size = SizeApproximation.add bound (transition,var) appr.size }

  let add_sizebounds bound scc appr =
    { appr with size = SizeApproximation.add_all bound scc appr.size }

  let is_size_bounded program appr t =
    not @@ VarSet.exists (fun v -> B.is_infinity @@ sizebound appr t v ) (Program.input_vars program)

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
    not % B.is_infinity % timebound appr

  (** Costbound related methods *)

  let costbound =
    TransitionApproximation.get % cost

  let program_costbound =
    TransitionApproximation.sum % cost

  let add_costbound bound transition appr =
    { appr with cost = TransitionApproximation.add bound transition appr.cost }

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
         mk_str_line ("Overall timebound:" ^ B.to_string ~pretty overall_timebound)
         <> TransitionApproximation.to_formatted ~pretty (Program.transitions program |> TransitionSet.to_list) appr.time) )

    <> mk_str_header_small "Costbounds" <> ( mk_paragraph (
          mk_str_line ("Overall costbound: " ^ B.to_string ~pretty (program_costbound appr program))
          <> TransitionApproximation.to_formatted ~pretty (Program.transitions program |> TransitionSet.to_list) appr.cost ) )

    <> mk_str_header_small "Sizebounds" <> (mk_paragraph @@ SizeApproximation.to_formatted ~pretty appr.size)


  (* TODO: use to_formatted *)
  let to_string program appr =
    let overall_costbound = program_costbound appr program in
    let output = IO.output_string () in
      if (not (B.is_infinity overall_costbound)) then
        IO.nwrite output ("YES( ?, " ^ B.to_string (overall_costbound) ^ ")\n\n")
      else
        IO.nwrite output "MAYBE\n\n";
      IO.nwrite output "Initial Complexity Problem After Preprocessing:\n";
      IO.nwrite output (Program.to_string program^"\n");
      IO.nwrite output "Timebounds: \n";
      IO.nwrite output ("  Overall timebound: " ^ B.to_string (program_timebound appr program) ^ "\n");
      appr.time |> TransitionApproximation.to_string (TransitionSet.to_list @@ Program.transitions program) |> IO.nwrite output;
      IO.nwrite output "\nCostbounds:\n";
      IO.nwrite output ("  Overall costbound: " ^ B.to_string (overall_costbound) ^ "\n");
      appr.cost |> TransitionApproximation.to_string (TransitionSet.to_list @@ Program.transitions program) |> IO.nwrite output;
      IO.nwrite output "\nSizebounds:\n";
      appr.size |> SizeApproximation.to_string |> IO.nwrite output;
      IO.close_out output
end

module Coerce(B: BoundType.Bound)
             (PM: ProgramTypes.ProgramModules)(PM': ProgramTypes.ProgramModules)
             (E: sig

                val t_eq: (PM.Transition.t,PM'.Transition.t) Util.TypeEq.t

                module RVTupleEq: functor(F: functor(_: ProgramTypes.RVTuple) -> sig type t end) -> sig
                  val proof: (F(PM.RV.RVTuple_).t, F(PM'.RV.RVTuple_).t) Util.TypeEq.t
                 end
              end) = struct

  module SizeApproximationEq = SizeApproximationType.EqMake(B)(PM.RV)(PM'.RV)(E.RVTupleEq)
  module TransitionApproximationEq = TransitionApproximationType.EqMake(B)(PM)(PM')

  let coerce: Make(B)(PM).t -> Make(B)(PM').t = fun appr ->
    match SizeApproximationEq.proof, TransitionApproximationEq.proof with
    | Refl,Refl ->
      (* type equality does not seem to be lifted to records *)
      { size = appr.size; time = appr.time; cost = appr.cost; }
end

module MakeForClassicalAnalysis(PM: ProgramTypes.ProgramModules) =
  Make(BoundsInst.Bound)(PM)

include MakeForClassicalAnalysis(ProgramModules)
