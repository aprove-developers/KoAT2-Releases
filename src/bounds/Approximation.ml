open Batteries

type kind = [ `Lower | `Upper ] [@@deriving eq]

let kind_to_string = function
  | `Upper -> "Upper"
  | `Lower -> "Lower"

module TimeMap = Hashtbl.Make(Program.Transition)

module SizeMap =
  Hashtbl.Make(
      struct
        type t = kind * Program.Transition.t * Var.t [@@deriving eq]
        let hash (kind, t, v) =
          Hashtbl.hash (kind_to_string kind
                        ^ Program.(Location.to_string (Transition.src t) ^ Location.to_string (Transition.target t))
                        ^ Var.to_string v)
      end
    )

type t = {
    time: Bound.t TimeMap.t;
    size: Bound.t SizeMap.t;
  }

let empty transitioncount varcount = {
    time = TimeMap.create (2 * transitioncount);
    size = SizeMap.create (2 * transitioncount * varcount);
  }

let timebounds_to_string appr =
  let output = IO.output_string () in
  TimeMap.print
    (fun output transition -> IO.nwrite output (Program.Transition.to_src_target_string transition))
    (fun output bound -> IO.nwrite output (Bound.to_string bound))
    output appr.time;  
  IO.close_out output

let sizebounds_to_string appr =
  let output = IO.output_string () in
  SizeMap.print
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

let timebound appr transition =
  TimeMap.find_option appr.time transition
  |? Bound.infinity

let timebound_graph appr graph =
  Program.TransitionGraph.fold_edges_e (fun transition -> Bound.add (timebound appr transition)) (Program.graph graph) Bound.zero

let add_timebound bound transition appr =
  (try
    TimeMap.modify transition (Bound.min bound) appr.time
  with Not_found -> TimeMap.add appr.time transition bound);
  appr
  
(* Returns the operator to combine two bounds with the best result. *)
let combine_bounds = function
  | `Lower -> Bound.max
  | `Upper -> Bound.min
           
let sizebound kind appr transition var =
  SizeMap.find_option appr.size (kind, transition, var)
  |? match kind with
     | `Lower -> Bound.minus_infinity
     | `Upper -> Bound.infinity       

let add_sizebound kind bound transition var appr =
  (try
    SizeMap.modify (kind, transition, var) (combine_bounds kind bound) appr.size
  with Not_found -> SizeMap.add appr.size (kind, transition, var) bound)
  ; appr      

let add_sizebounds kind bound scc appr =
  List.iter (fun (t,v) -> ignore (add_sizebound kind bound t v appr)) scc;
  appr
