open Batteries

type kind = [ `Lower | `Upper ]

let kind_to_string = function
  | `Upper -> "Upper"
  | `Lower -> "Lower"

type t = {
    time: ((kind * Program.Transition.t), Bound.t) Hashtbl.t;
    size: ((kind * Program.Transition.t * Var.t), Bound.t) Hashtbl.t;
  }

let empty transitioncount varcount = {
    time = Hashtbl.create (2 * transitioncount);
    size = Hashtbl.create (2 * transitioncount * varcount);
  }

let timebounds_to_string appr =
  let output = IO.output_string () in
  Hashtbl.print
    (fun output (kind, transition) -> IO.nwrite output (Program.Transition.to_src_target_string transition))
    (fun output bound -> IO.nwrite output (Bound.to_string bound))
    output appr.time;  
  IO.close_out output

let sizebounds_to_string appr =
  let output = IO.output_string () in
  Hashtbl.print
    (fun output (kind, transition, var) -> IO.nwrite output (kind_to_string kind ^ ": (" ^ Program.Transition.to_src_target_string transition ^ ", " ^ Var.to_string var ^ ")"))
    (fun output bound -> IO.nwrite output (Bound.to_string bound))
    output appr.size;
  IO.close_out output

let to_string appr =
  let output = IO.output_string () in
  IO.nwrite output "Timebounds: ";
  IO.nwrite output (timebounds_to_string appr);
  IO.nwrite output "\nSizebounds: ";
  IO.nwrite output (sizebounds_to_string appr);
  IO.close_out output

(* Returns the operator to combine two bounds with the best result. *)
let combine_bounds = function
  | `Lower -> Bound.max
  | `Upper -> Bound.min
           
let timebound kind appr transition =
  Hashtbl.find_option appr.time (kind, transition)
  |? match kind with
     | `Lower -> Bound.zero
     | `Upper -> Bound.infinity

let timebound_graph kind appr graph =
  match kind with
  | `Lower -> Bound.one
  | `Upper -> Program.TransitionGraph.fold_edges_e (fun transition -> Bound.add (timebound `Upper appr transition)) (Program.graph graph) Bound.zero

let add_timebound kind bound transition appr =
  (try
    Hashtbl.modify (kind, transition) (combine_bounds kind bound) appr.time
  with Not_found -> Hashtbl.add appr.time (kind, transition) bound);
  appr
  
let sizebound kind appr transition var =
  Hashtbl.find_option appr.size (kind, transition, var)
  |? match kind with
     | `Lower -> Bound.minus_infinity
     | `Upper -> Bound.infinity       

let add_sizebound kind bound transition var appr =
  (try
    Hashtbl.modify (kind, transition, var) (combine_bounds kind bound) appr.size
  with Not_found -> Hashtbl.add appr.size (kind, transition, var) bound)
  ; appr      

let add_sizebounds kind bound scc appr =
  List.iter (fun (t,v) -> ignore (add_sizebound kind bound t v appr)) scc;
  appr
