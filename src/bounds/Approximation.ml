open Batteries
open ProgramTypes
   
type kind = [ `Lower | `Upper ] [@@deriving eq, ord, show]

let logger = Logging.(get Approximation) 

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

let sizebound kind =
  SizeApproximation.get kind % size

let add_sizebound kind bound transition var appr =
  { appr with size = SizeApproximation.add kind bound transition var appr.size }

let add_sizebounds kind bound scc appr =
  { appr with size = SizeApproximation.add_all kind bound scc appr.size }

(** Timebound related methods *)

let timebound =
  TransitionApproximation.get % time

let timebound_id =
  TransitionApproximation.get_id % time

let program_timebound =
  TransitionApproximation.sum % time

let add_timebound bound transition appr =
  let label = Transition.label transition in
  let temp_vars = VarSet.diff (TransitionLabel.vars label) (TransitionLabel.input_vars label) in
  let temp_bound = fun kind var -> if (VarSet.mem var temp_vars) then sizebound kind appr transition var else Bound.of_var var in
  let replaced_bound = Bound.appr_substitution `Upper ~lower:(temp_bound `Lower) ~higher:(temp_bound `Upper) bound in
  { appr with time = TransitionApproximation.add replaced_bound transition appr.time }

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
  
let to_string ?(html=false) program appr =
  let begin_head = if html then "<h3>" else "" in
  let end_head = if html then "</h3>" else "" in

  let endline = if html then "<br>" else "\n" in
  let overall_timebound = program_timebound appr program in 
  let output = IO.output_string () in
    let asym_timebound = Bound.asymptotic_complexity overall_timebound in
      IO.nwrite output ((Bound.show_complexity_termcomp asym_timebound) ^ endline ^ endline);
      IO.nwrite output (begin_head ^ "Initial Complexity Problem:" ^ end_head ^ endline);
      IO.nwrite output (Program.to_string ~html:html program ^ endline);
      IO.nwrite output (begin_head ^ "Timebounds:" ^ end_head ^ endline);
      IO.nwrite output ("  Overall timebound: " ^ Bound.to_string (overall_timebound) ^ endline);
      appr.time |> TransitionApproximation.to_string ~html:html (Program.transitions program) |> IO.nwrite output;
      IO.nwrite output (endline ^ begin_head ^ "Costbounds:" ^ end_head ^ endline);
      IO.nwrite output ("  Overall costbound: " ^ Bound.to_string (program_costbound appr program) ^ endline);
      appr.cost |> TransitionApproximation.to_string ~html:html (Program.transitions program) |> IO.nwrite output;
      IO.nwrite output (endline ^ begin_head ^ "Sizebounds:" ^ end_head ^ endline);
      appr.size |> SizeApproximation.to_string ~html:html |> IO.nwrite output;
      IO.close_out output
