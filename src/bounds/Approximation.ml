open Batteries
open Program.Types
   
type kind = [ `Lower | `Upper ] [@@deriving eq, ord, show]

let logger = Logging.(get Approximation) 

type t = {
    time: TransitionApproximation.t;
    size: SizeApproximation.t
  }

let equivalent appr1 appr2 =
  TransitionApproximation.equivalent appr1.time appr2.time
  && SizeApproximation.equivalent appr1.size appr2.size
  
let empty transitioncount varcount = {
    time = TransitionApproximation.empty "time" transitioncount;
    size = SizeApproximation.empty (2 * transitioncount * varcount);
  }

let create program =
  empty (TransitionGraph.nb_edges (Program.graph program))
        (VarSet.cardinal (Program.vars program))

let time appr = appr.time

let size appr = appr.size

let costbound appr program =
  TransitionGraph.fold_edges_e (fun transition result ->
      Bound.(TransitionApproximation.get appr.time transition * of_poly (Transition.cost transition) + result)
    ) (Program.graph program) Bound.zero

let timebound =
  TransitionApproximation.get % time

let timebound_between =
  TransitionApproximation.get_between % time
                        
let add_timebound bound transition appr =
  { appr with time = TransitionApproximation.add bound transition appr.time }

let all_times_bounded =
  TransitionApproximation.all_bounded % time
                                   
let sizebound kind =
  SizeApproximation.get kind % size

let add_sizebound kind bound transition var appr =
  { appr with size = SizeApproximation.add kind bound transition var appr.size }

let add_sizebounds kind bound scc appr =
  { appr with size = SizeApproximation.add_all kind bound scc appr.size }

let to_string program appr =
  let output = IO.output_string () in
  IO.nwrite output "Timebounds: \n";
  IO.nwrite output ("  Overall timebound: " ^ Bound.to_string (TransitionApproximation.sum appr.time program) ^ "\n");
  IO.nwrite output ("  Overall costbound: " ^ Bound.to_string (costbound appr program) ^ "\n");
  appr.time |> TransitionApproximation.to_string |> IO.nwrite output;
  IO.nwrite output "\nSizebounds:\n";
  appr.size |> SizeApproximation.to_string |> IO.nwrite output;
  IO.close_out output
