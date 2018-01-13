open Batteries
open Program.Types
   
type kind = [ `Lower | `Upper ] [@@deriving eq, ord, show]

let logger = Logging.(get Approximation) 

type t = TimeApproximation.t * SizeApproximation.t

let equivalent (time1,size1) (time2,size2) =
  TimeApproximation.equivalent time1 time2
  && SizeApproximation.equivalent size1 size2
  
let empty transitioncount varcount =
    TimeApproximation.empty transitioncount,
    SizeApproximation.empty (2 * transitioncount * varcount)

let create program =
  empty (TransitionGraph.nb_edges (Program.graph program)) (VarSet.cardinal (Program.vars program))

let time (time, _) = time

let size (_, size) = size

let costbound (time, _) program =
  TransitionGraph.fold_edges_e (fun transition result -> Bound.(TimeApproximation.get time transition * of_poly (Transition.cost transition) + result)) (Program.graph program) Bound.zero

let timebound (time, _) = TimeApproximation.get time

let timebound_between (time, _) = TimeApproximation.get_between time
                        
let add_timebound bound transition = Tuple2.map1 (TimeApproximation.add bound transition)

let all_times_bounded (time, _) = TimeApproximation.all_bounded time
                                   
let sizebound kind (_, size) = SizeApproximation.get kind size

let add_sizebound kind bound transition var = Tuple2.map2 (SizeApproximation.add kind bound transition var)

let add_sizebounds kind bound scc = Tuple2.map2 (SizeApproximation.add_all kind bound scc)

let to_string program (time, size) =
  let output = IO.output_string () in
  IO.nwrite output "Timebounds: \n";
  IO.nwrite output ("  Overall timebound: " ^ Bound.to_string (TimeApproximation.sum time program) ^ "\n");
  IO.nwrite output ("  Overall costbound: " ^ Bound.to_string (costbound (time, size) program) ^ "\n");
  time |> TimeApproximation.to_string |> IO.nwrite output;
  IO.nwrite output "\nSizebounds:\n";
  size |> SizeApproximation.to_string |> IO.nwrite output;
  IO.close_out output
