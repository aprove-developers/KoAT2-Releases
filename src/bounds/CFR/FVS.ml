open! OurBase

let cfr_logger = Logging.(get CFR)

module FVS (PM : ProgramTypes.ProgramModules) = struct
  module Cycles = Cycles.Cycles (PM)
  open PM

  exception FVSFailed

  (** Compute the Feedback-Vertex Set for a given graph, i.e., a set of vertices such that it contains at least one vertex of any cycle in the graph.
      If se t of all cycles have already been computed, it can be given reused to avoid recomputations. *)
  let fvs ?(cycles = None) graph =
    let cfg = [ ("model", "true"); ("proof", "false"); ("timeout", "2000") ] in

    let ctx = Z3.mk_context cfg in

    let cycles =
      match cycles with
      | Some lps -> lps
      | None -> Cycles.find_cycles graph
    in

    let locations = TransitionGraph.locations graph in

    (* Initialize hashtable. Here, we use the number of locations as the initial size. *)
    let location_var_map = Hashtbl.create ~size:(Set.length locations) (module Location) in

    (* Creates and stores a (Z3) integer variable for every location. These variables
       represent those locations which are part of the FVS. *)
    Set.iter
      ~f:(fun location ->
        let ivar = location |> Location.to_string |> Z3.Arithmetic.Integer.mk_const_s ctx in
        Hashtbl.add_exn location_var_map ~key:location ~data:ivar)
      locations;

    (* Returns the integer variable of a given location. *)
    let var_for location = Hashtbl.find_exn location_var_map location in

    (* Z3 zero. *)
    let zero = Z3.Arithmetic.Integer.mk_numeral_i ctx 0 and one = Z3.Arithmetic.Integer.mk_numeral_i ctx 1 in

    (* Creates an optimization problem. *)
    let opt = Z3.Optimize.mk_opt ctx in

    (* Every variable should only take values {0, 1} *)
    Hashtbl.data location_var_map
    |> List.fold
         ~f:(fun constraints var ->
           let ge0 = Z3.Arithmetic.mk_ge ctx var zero and le1 = Z3.Arithmetic.mk_le ctx var one in
           ge0 :: le1 :: constraints)
         ~init:[]
    |> Z3.Optimize.add opt;

    (* Every cycle must contain a marked location (i.e., its variable gets the value 1) location. *)
    List.fold
      ~f:(fun constraints cycle ->
        let cycle_constraint =
          List.fold ~f:(fun expressions location -> var_for location :: expressions) ~init:[] cycle
          |> Z3.Arithmetic.mk_add ctx |> Z3.Arithmetic.mk_le ctx one
        in
        cycle_constraint :: constraints)
      ~init:[] cycles
    |> Z3.Optimize.add opt;

    (* Solve ILP, minimizing the number of marked locations. *)
    let _handle = Hashtbl.data location_var_map |> Z3.Arithmetic.mk_add ctx |> Z3.Optimize.minimize opt in

    Logging.log ~level:Logger.DEBUG cfr_logger "fvs" (fun () ->
        [ ("Z3_INPUT", "\n" ^ Z3.Optimize.to_string opt) ]);

    match Z3.Optimize.check opt with
    | Z3.Solver.UNSATISFIABLE
    | Z3.Solver.UNKNOWN ->
        raise FVSFailed
    | Z3.Solver.SATISFIABLE ->
        ();
        let model =
          match Z3.Optimize.get_model opt with
          | Some m -> m
          | None -> raise FVSFailed
        in
        Logging.log ~level:Logger.DEBUG cfr_logger "fvs" (fun () ->
            [ ("Z3_MODEL", "\n" ^ Z3.Model.to_string model) ]);
        let fvs_solution =
          Hashtbl.fold
            ~f:(fun ~key:location ~data:var fvs_solution ->
              match Z3.Model.get_const_interp_e model var with
              | Some value ->
                  if Z3.Expr.equal value one then
                    Set.add fvs_solution location
                  else
                    fvs_solution
              | None -> raise FVSFailed)
            location_var_map ~init:LocationSet.empty
        in
        Logging.log ~level:Logger.INFO cfr_logger "fvs" (fun () ->
            [ ("FVS", LocationSet.to_string fvs_solution) ]);
        fvs_solution
end
