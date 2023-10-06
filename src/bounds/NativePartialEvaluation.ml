open! OurBase

let cfr_logger = Logging.(get CFR)

let log ?(level = Logger.INFO) method_name data =
  Logger.log cfr_logger level (fun () -> (method_name, data ()))


type config = { abstract : [ `FVS | `LoopHeads ]; k_encounters : int; update_invariants : bool }

module Loops (PM : ProgramTypes.ProgramModules) = struct
  open PM

  let loop_to_string loop = Sequence.of_list loop |> Util.sequence_to_string ~f:Location.to_string
  let loops_to_string loops = Sequence.of_list loops |> Util.sequence_to_string ~f:loop_to_string

  (* mutable state *)
  type state = { blocked : (Location.t, bool) Hashtbl.t; b_lists : (Location.t, Location.t list) Hashtbl.t }

  (** Finds all loops in a graph, using the algorithm from Donald B. Johnson (1975)
  By itself this function is probably not very useful. Use transition_loops_for in
  order to get the loops with transitions.
  *)
  let find_loops graph =
    let all_locations = TransitionGraph.locations graph in

    (* initialize hashtables to the number of locations in the graph *)
    let hashtbl_size = Set.length all_locations in

    (* Initial state for circuit, based on a set of locations *)
    let initial_state locations =
      let state =
        {
          blocked = Hashtbl.create ~size:hashtbl_size (module Location);
          b_lists = Hashtbl.create ~size:hashtbl_size (module Location);
        }
      in
      Set.iter
        ~f:(fun location ->
          Hashtbl.add_exn state.blocked ~key:location ~data:false;
          Hashtbl.add_exn state.b_lists ~key:location ~data:[])
        locations;
      state
    in

    (* Check if a location is blocked, doesn't mutate s *)
    let is_blocked s location = Option.value ~default:false (Hashtbl.find s.blocked location) in

    (* Get the b_list of a location, doesn't mutate s *)
    let b_list_of s location = Option.value ~default:[] (Hashtbl.find s.b_lists location) in

    (* Add a location v to the b_list of w, if not already present; mutates s *)
    let add_to_b_list s w v =
      let old_b_list = b_list_of s w in
      if not (List.mem ~equal:Location.equal old_b_list v) then
        Hashtbl.update s.b_lists w ~f:(fun _ -> v :: old_b_list)
    in

    (* Block a location, mutates s *)
    let block s location = Hashtbl.update s.blocked location ~f:(fun _ -> true) in

    (* Unblock a location, and clear b_lists, mutates s *)
    let rec unblock s location =
      if is_blocked s location then (
        Hashtbl.update s.blocked location ~f:(fun _ -> false);
        List.iter
          ~f:(fun l ->
            log ~level:Logger.DEBUG "johnson" (fun () -> [ ("UNBLOCK", Location.to_string l) ]);
            unblock s l)
          (Option.value ~default:[] (Hashtbl.find s.b_lists location));
        Hashtbl.update s.b_lists location ~f:(fun _ -> []))
    in

    (* Transform the path into a list of locations, without mutating the path *)
    let loop_of = Stack.to_list in

    (* We start with an empty path *)
    let path = Stack.create () in

    let rec circuit graph s prev_results start_location current_location =
      Stack.push path current_location;
      log ~level:Logger.DEBUG "johnson" (fun () -> [ ("BLOCK", Location.to_string current_location) ]);
      block s current_location;
      (* TransitionGraph.succ can contain repetitions *)
      let successors =
        TransitionGraph.succ graph current_location
        |> LocationSet.of_list
        |> tap (fun ls ->
               log ~level:Logger.DEBUG "johnson" (fun () ->
                   [
                     ("CURRENT", Location.to_string current_location); ("SUCCESSORS", LocationSet.to_string ls);
                   ]))
      in
      let closed, new_results =
        Set.fold
          ~f:(fun (closed_acc, loops_acc) next_location ->
            if Location.equal next_location start_location then (
              let loop = loop_of path in
              log ~level:Logger.DEBUG "johnson" (fun () -> [ ("FOUND_LOOP", loop_to_string loop) ]);
              (* Found a loop, add to results *)
              (true, loop :: loops_acc))
            else if not (is_blocked s next_location) then
              let inner_closed, inner_loops_acc = circuit graph s loops_acc start_location next_location in
              (closed_acc || inner_closed, inner_loops_acc)
            else
              (closed_acc, loops_acc))
          successors ~init:(false, prev_results)
      in

      if closed then
        unblock s current_location
      else
        TransitionGraph.iter_succ (fun w -> add_to_b_list s w current_location) graph current_location;

      let _ = Stack.pop path in
      (closed, new_results)
    in

    (* Locations are ordered; as is the LocationSet *)
    let _, results =
      Set.fold
        ~f:(fun (current_graph, prev_results) location ->
          log ~level:Logger.DEBUG "johnson" (fun () -> [ ("MIN_LOCATION", Location.to_string location) ]);
          (* The SCC containing the smallest location according to the ordering *)
          let min_scc_opt =
            List.find (TransitionGraph.sccs current_graph) ~f:(fun scc ->
                Set.mem (TransitionSet.locations scc) location)
          in
          log ~level:Logger.DEBUG "johnson" (fun () ->
              [ ("MIN_SCC", min_scc_opt |> Option.map ~f:TransitionSet.to_string |? "None") ]);
          (* the smallest location might be in a trivial scc and already filtered by TransitionGraph.sccs *)
          let new_results =
            match min_scc_opt with
            | Some scc ->
                let scc_graph = TransitionGraph.mk (Set.to_sequence scc)
                and scc_locations = TransitionSet.locations scc in
                let _closed, results =
                  circuit scc_graph (initial_state scc_locations) prev_results location location
                in
                results
            | None -> prev_results
          in
          (TransitionGraph.remove_vertex current_graph location, new_results))
        (TransitionGraph.locations graph) ~init:(graph, [])
    in
    log ~level:Logger.INFO "loops" (fun () -> [ ("LOOPS", loops_to_string results) ]);
    results


  (** Find all loops in a given scc using the algorithm from Donald B. Johnson (1975) **)
  let find_loops_scc graph scc =
    let scc_graph = Set.to_sequence scc |> TransitionGraph.mk in
    find_loops scc_graph


  (** For a list of location loops, this function creates the list of all transition loops
      containing a loop from the location loops.

      Example:
      The graph G contains transitions
      (l0, t0, l1)
      (l1, t1, l2)
      (l1, t2, l2)
      (l2, t3, l1)

      The loop detection `find_loops` would only find the loop [l1, l2].
      This function expands the (location) loop [l1,l2] the the transition loops
      [t1,t3], [t2,t3].

      *)
  let transition_loops_from graph (loc_loops : Location.t list list) =
    let transitions_betwen_locations src target =
      TransitionGraph.fold_succ_e
        (fun t ts ->
          if Location.equal (Transition.target t) target then
            t :: ts
          else
            ts)
        graph src []
    in

    let combine (transitions : Transition.t list) (suffixes : Transition.t list list) : Transition.t list list
        =
      List.fold
        ~f:(fun results suffix ->
          List.fold ~f:(fun results transition -> (transition :: suffix) :: results) ~init:[] transitions)
        ~init:[] suffixes
    in

    (* computes for every step in the loop the walkable transitions *)
    let rec transition_loops (lh : Location.t) (loc_loop : Location.t list) =
      match loc_loop with
      | l1 :: l2 :: ls -> combine (transitions_betwen_locations l1 l2) (transition_loops lh (l2 :: ls))
      | l1 :: [] ->
          (* start loops with transitions from last location to head *)
          combine (transitions_betwen_locations l1 lh) [ [] ]
      | [] -> [ [] ]
    in

    List.fold
      ~f:(fun results loc_loop -> List.append (transition_loops (List.hd_exn loc_loop) loc_loop) results)
      ~init:[] loc_loops


  (** Rotate a loop, so that its head is the first location *)
  let rotate head loop =
    let rec rotate_ prefix suffix =
      log ~level:Logger.DEBUG "rotate" (fun () ->
          [ ("PREFIX", loop_to_string prefix); ("SUFFIX", loop_to_string suffix) ]);
      match suffix with
      | location :: suffix ->
          if location == head then
            (* attach prefix at the end *)
            List.append (location :: suffix) (List.rev prefix)
          else
            (* continue rotating *)
            rotate_ (location :: prefix) suffix
      | [] -> List.rev prefix
    in
    rotate_ [] loop


  (** Returns the first locations of the loops *)
  let loop_heads loops =
    List.fold ~f:(fun loop_heads loop -> Set.add loop_heads (List.hd_exn loop)) ~init:LocationSet.empty loops
end

module FVS (PM : ProgramTypes.ProgramModules) = struct
  module Loops = Loops (PM)
  (* TODO: move this module to SMT module *)

  open PM

  exception FVSFailed

  (** Compute the Feedback-vertex Set for a given graph. If loops have already
      been computed you can give them to this function to avoid recomputation.
      *)
  let fvs ?(loops = None) graph =
    let cfg =
      [
        ("model", "true");
        ("proof", "false");
        ("timeout", "2000");
        (* timeout (unsigned) default timeout (in milliseconds) used for solvers *)
      ]
    in

    let ctx = Z3.mk_context cfg in

    let loops =
      match loops with
      | Some lps -> lps
      | None -> Loops.find_loops graph
    in

    let locations = TransitionGraph.locations graph in

    (* initialize hashtables to the number of locations in the graph *)
    let location_var_map = Hashtbl.create ~size:(Set.length locations) (module Location) in

    (* create and remember a (z3) integer variable for every location. it will
       represent if the location is part of the FVS or Hitting Set *)
    Set.iter
      ~f:(fun location ->
        let ivar = location |> Location.to_string |> Z3.Arithmetic.Integer.mk_const_s ctx in
        Hashtbl.add_exn location_var_map ~key:location ~data:ivar)
      locations;

    (* get the integer variable representing a location *)
    let var_for location = Hashtbl.find_exn location_var_map location in

    let zero = Z3.Arithmetic.Integer.mk_numeral_i ctx 0 and one = Z3.Arithmetic.Integer.mk_numeral_i ctx 1 in

    (* create an optimization problem *)
    let o = Z3.Optimize.mk_opt ctx in

    (* restrict every variable to be in {0, 1} *)
    Hashtbl.data location_var_map
    |> List.fold
         ~f:(fun constraints var ->
           let ge0 = Z3.Arithmetic.mk_ge ctx var zero and le1 = Z3.Arithmetic.mk_le ctx var one in
           ge0 :: le1 :: constraints)
         ~init:[]
    |> Z3.Optimize.add o;

    (* require every loop to contain one marked/hit location *)
    List.fold
      ~f:(fun constraints loop ->
        let loop_constraint =
          List.fold ~f:(fun expressions location -> var_for location :: expressions) ~init:[] loop
          |> Z3.Arithmetic.mk_add ctx
          (* |> tap (fun e -> Z3.Expr.to_string e |> String.println stdout ) *)
          |> Z3.Arithmetic.mk_le ctx one
        in
        loop_constraint :: constraints)
      ~init:[] loops
    |> Z3.Optimize.add o;

    (* Solve ILP, minimizing the number of marked locations *)
    let _handle = Hashtbl.data location_var_map |> Z3.Arithmetic.mk_add ctx |> Z3.Optimize.minimize o in

    log ~level:Logger.DEBUG "fvs" (fun () -> [ ("Z3_INPUT", "\n" ^ Z3.Optimize.to_string o) ]);

    match Z3.Optimize.check o with
    | Z3.Solver.UNSATISFIABLE
    | Z3.Solver.UNKNOWN ->
        raise FVSFailed
    | Z3.Solver.SATISFIABLE ->
        ();

        let model =
          match Z3.Optimize.get_model o with
          | Some m -> m
          | None -> raise FVSFailed
        in

        log ~level:Logger.DEBUG "fvs" (fun () -> [ ("Z3_MODEL", "\n" ^ Z3.Model.to_string model) ]);

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

        log ~level:Logger.INFO "fvs" (fun () -> [ ("FVS", LocationSet.to_string fvs_solution) ]);

        fvs_solution
end

(* TODO: Move to ProgramModules and/or Polynomials *)

(** Generic adapter for program modules with generic overapproximation *)
module type Adapter = sig
  type update_element
  type transition_label
  type transition_graph
  type program
  type location
  type approx = Polynomials.Polynomial.t * Guard.t

  val overapprox_indeterminates : update_element -> approx
  val location_with_index : int -> location -> location
  val create_new_program : location -> transition_graph -> program
end

(* TODO: Move to ProgramModules and/or Polynomials *)

(** Trivial implementation of overapproxmation in classical programs *)
module ClassicAdapter :
  Adapter
    with type update_element = ProgramModules.UpdateElement.t
     and type transition_label = ProgramModules.TransitionLabel.t
     and type location = ProgramModules.Location.t
     and type transition_graph = ProgramModules.TransitionGraph.t
     and type program = ProgramModules.Program.t = struct
  open ProgramModules

  type update_element = UpdateElement.t
  type approx = Polynomials.Polynomial.t * Guard.t
  type transition_label = TransitionLabel.t
  type location = Location.t
  type transition_graph = TransitionGraph.t
  type program = Program.t

  (** Overapproximating of normal polynomials is not required and the polynomial is returned as is *)
  let overapprox_indeterminates poly = (poly, Guard.mk_true)

  let location_with_index index location =
    Printf.sprintf "%s_%i" (Location.to_string location) index |> Location.of_string


  let create_new_program = Program.from_graph
end

(* TODO: Move to ProgramModules and/or Polynomials *)

(** Use already existing overapproximation *)
module ProbabilisticAdapter :
  Adapter
    with type update_element = ProbabilisticProgramModules.UpdateElement.t
     and type transition_label = ProbabilisticProgramModules.TransitionLabel.t
     and type location = ProbabilisticProgramModules.Location.t
     and type transition_graph = ProbabilisticProgramModules.TransitionGraph.t
     and type program = ProbabilisticProgramModules.Program.t = struct
  open ProbabilisticProgramModules

  type update_element = UpdateElement.t
  type approx = Polynomials.Polynomial.t * Guard.t
  type transition_label = TransitionLabel.t
  type location = Location.t
  type transition_graph = TransitionGraph.t
  type program = Program.t

  module P = Polynomials.Polynomial

  let overapprox_indeterminates ue =
    let new_var = Var.fresh_id Var.Int () in
    (P.of_var new_var, UpdateElement_.as_guard ue new_var)


  let location_with_index index location =
    Printf.sprintf "%s_%i" (Location.to_string location) index |> Location.of_string


  let create_new_program start graph =
    Set.to_sequence (TransitionGraph.transitions graph)
    |> Sequence.map ~f:Transition.gt |> GeneralTransitionSet.of_sequence |> Program.from_gts start
end

module ApronUtils (PM : ProgramTypes.ProgramModules) = struct
  open Constraints
  open Polynomials
  open PM
  module VarMap = MakeMapCreators1 (Var)

  type polyhedron
  type guard = Constraint.t
  type update = UpdateElement.t VarMap.t

  (* TODO: move to ApronInterface *)

  (** Converts an apron scalar into its koat equivalent *)
  let scalar_from_apron (scalar : Apron.Scalar.t) : OurInt.t =
    let open Apron in
    match scalar with
    | Scalar.Float float -> (OurInt.of_int % int_of_float) float
    | Scalar.Mpqf float -> (OurInt.of_int % int_of_float % Mpqf.to_float) float
    | Scalar.Mpfrf float -> (OurInt.of_int % int_of_float % Mpfrf.to_float) float


  (* TODO: move to K2A *)

  (** Creates constraints for a variable bounded by an interval *)
  let interval_from_apron (var : Var.t) (interval : Apron.Interval.t) : Constraint.t =
    let constr = ref [] in
    if Apron.Scalar.is_infty interval.sup = 0 then
      constr :=
        Constraint.Infix.(
          interval.sup |> scalar_from_apron |> Polynomial.of_constant >= Polynomial.of_var var)
        :: !constr;
    if Apron.Scalar.is_infty interval.inf = 0 then
      constr :=
        Constraint.Infix.(
          interval.inf |> scalar_from_apron |> Polynomial.of_constant <= Polynomial.of_var var)
        :: !constr;
    Constraint.all !constr


  (* TODO: move to ApronInterface *)

  (** converts an update map over polynomials to apron arrays *)
  let update_to_apron env (update : Polynomials.Polynomial.t VarMap.t) =
    Map.to_sequence update
    |> Sequence.map ~f:(fun (var, ue) ->
           let apron_var = ApronInterface.Koat2Apron.var_to_apron var
           and apron_expr = ApronInterface.Koat2Apron.poly_to_apron env ue in
           (apron_var, apron_expr))
    |> Sequence.to_array |> Array.unzip


  (** Adapter for polyheron operations used directly on constraints *)
  let guard_to_polyh am guard =
    let env = Apron.Environment.make (ApronInterface.Koat2Apron.vars_to_apron (Constraint.vars guard)) [||] in
    let tcons_array = ApronInterface.Koat2Apron.constraint_to_apron env guard in
    let polyh = Apron.Abstract1.of_tcons_array am env tcons_array in
    polyh


  (** Adapter for polyheron operations used directly on constraints *)
  let polyh_to_guard am polyh =
    (* Weird side-effect to get a clean output of constraints *)
    Apron.Abstract1.minimize am polyh;
    polyh |> Apron.Abstract1.to_tcons_array am |> ApronInterface.Apron2Koat.constraint_from_apron


  (*
    For an abstract domain `'a` there is a manager `'a Manager.t` handling the
    library stuff underneath. It is not important to this implementation, but
    is required for most of the operations. We will use the Ppl abstract domain
    for partial evaluation, but others might be used at will.

    A value in the abstract domain `'a` is an `'a Abstract.t`. It holds a reference
    to the environment (variables).
  *)

  (** add new dimensions to to an abstract value, ignores already existing dimenstions *)
  let add_vars_to_polyh am vars polyh =
    let prev_env = Apron.Abstract1.env polyh in
    let prev_vars = Apron.Environment.vars prev_env |> fst |> ApronInterface.Apron2Koat.vars_from_apron in
    (* Apron would panic if the variable is alread in the environment *)
    let new_vars = Set.filter ~f:(fun var -> not (Set.mem prev_vars var)) vars in

    (* log ~level:Logger.DEBUG "add_vars_to_polyh" (fun () -> *)
    (*     [ *)
    (*       ("REQUESTED_VARS", VarSet.to_string vars); *)
    (*       ("PRESENT_VARS", VarSet.to_string prev_vars); *)
    (*       ("NEW_VARS", VarSet.to_string new_vars); *)
    (*     ]); *)
    let new_env = Apron.Environment.add prev_env (ApronInterface.Koat2Apron.vars_to_apron new_vars) [||] in

    Apron.Abstract1.change_environment am polyh new_env false


  (** project a polyhedron onto the given variable dimensions
  Does not add new dimensions for variables not in the polyhedron
   *)
  let project_polyh am vars polyh =
    (* let keep_vars = ApronInterface.Koat2Apron.vars_to_apron vars *)
    (* and prev_env = Apron.Abstract1.env polyh in *)
    (* let forget_vars = *)
    (*   Apron.Environment.vars prev_env *)
    (*   |> fst *)
    (*   |> Array.filter (fun v -> not (Array.mem v keep_vars)) *)
    (* in *)
    (* Apron.Abstract1.forget_array am polyh forget_vars false *)
    let prev_env = Apron.Abstract1.env polyh in
    let apron_vars =
      ApronInterface.Koat2Apron.vars_to_apron vars |> Array.filter ~f:(Apron.Environment.mem_var prev_env)
    in
    let new_env = Apron.Environment.make apron_vars [||] in
    (* Implicitly projects all removed dimensions *)
    Apron.Abstract1.change_environment am polyh new_env false


  (* |> tap (fun projected -> *)
  (*        log ~level:Logger.DEBUG "project" (fun () -> *)
  (*            [ *)
  (*              ( "POLYH", *)
  (*                polyh_to_guard am polyh |> Guard.to_string ~pretty:true ); *)
  (*              ("VARS", VarSet.to_string ~pretty:true vars); *)
  (*              ( "PROJECTED", *)
  (*                polyh_to_guard am projected |> Guard.to_string ~pretty:true *)
  (*              ); *)
  (*            ])) *)

  let project_guard am vars guard =
    guard_to_polyh am guard |> project_polyh am vars
    (* |> tap (fun p -> Apron.Abstract1.print Format.std_formatter p) *)
    |> polyh_to_guard am


  (** Intersect a polynomial with a constraint *)
  let meet am constr polyh =
    (* log ~level:Logger.DEBUG "meet" (fun () -> *)
    (*     [ ("CONSTR", Guard.to_string constr) ]); *)
    let env = Apron.Abstract1.env polyh in
    let apron_expr = ApronInterface.Koat2Apron.constraint_to_apron env constr in
    Apron.Abstract1.meet_tcons_array am polyh apron_expr


  (** Find bounds of the form x ⋄ c for variables x and bounds c *)
  let bound_variables_polyh am vars polyh =
    let env = Apron.Abstract1.env polyh in
    Set.to_sequence vars
    |> Sequence.map ~f:(fun variable ->
           let apron_var = ApronInterface.Koat2Apron.var_to_apron variable in
           if Apron.Environment.mem_var env apron_var then
             Apron.Abstract1.bound_variable am polyh apron_var |> interval_from_apron variable
           else
             Guard.mk_true)
    |> Sequence.fold ~f:Constraint.mk_and ~init:Constraint.mk_true


  let bound_variables_guard am vars guard = guard_to_polyh am guard |> bound_variables_polyh am vars

  let vars_in_update update =
    Map.fold
      ~f:(fun ~key:var ~data:ue vars -> Set.add vars var |> Set.union (Polynomials.Polynomial.vars ue))
      update ~init:VarSet.empty


  let update_polyh am update polyh =
    let vars_in_update =
      Map.fold ~f:(fun ~key:_ ~data:poly -> Set.union (Polynomial.vars poly)) update ~init:VarSet.empty
      |> Set.union (VarSet.of_list @@ Map.keys update)
    in
    let polyh_with_new_vars = add_vars_to_polyh am vars_in_update polyh in
    let env = Apron.Abstract1.env polyh_with_new_vars in
    let vars_arr, texpr_arr = update_to_apron env update in
    Apron.Abstract1.assign_texpr_array am polyh_with_new_vars vars_arr texpr_arr None


  let update_guard am update guard = guard_to_polyh am guard |> update_polyh am update |> polyh_to_guard am
end

module OverApproximationUtils (A : Adapter) = struct
  open A

  let overapprox_update update =
    Map.fold
      ~f:(fun ~key:var ~data:ue (new_update, guards) ->
        let ue_approx, guard = overapprox_indeterminates ue in
        (Map.add_exn ~key:var ~data:ue_approx new_update, Guard.mk_and guards guard))
      update
      ~init:(Map.empty (module Var), Guard.mk_true)
end

module Unfolding
    (PM : ProgramTypes.ProgramModules)
    (A : Adapter
           with type update_element = PM.UpdateElement.t
            and type transition_label = PM.TransitionLabel.t) =
struct
  open OverApproximationUtils (A)
  open ApronUtils (PM)

  (** [initial_guard_polyh am constr guard] computes a polyhedron overapproximating satisfying assignments for the conjunction of [constr] and [guard] *)
  let initial_guard_polyh am constr guard =
    Guard.mk_and constr guard
    |> tap (fun p ->
           log ~level:Logger.DEBUG "unfold" (fun () -> [ ("INITIAL", p |> Guard.to_string ~pretty:true) ]))
    |> guard_to_polyh am


  (** Unfolds the guard with an update, returns None if the guard was UNSAT.*)
  let unfold_update am initial_polyh program_vars update =
    let update_approx, update_guard = overapprox_update update in
    let update_temp_vars =
      [ Guard.vars update_guard; vars_in_update update_approx ]
      |> VarSet.union_list
      |> Set.filter ~f:(fun v -> not (Set.mem program_vars v))
    in
    let update_program_vars =
      [ Guard.vars update_guard; vars_in_update update_approx ]
      |> VarSet.union_list
      |> Set.filter ~f:(fun v -> Set.mem program_vars v)
    in

    initial_polyh
    |> add_vars_to_polyh am (Set.union update_program_vars update_temp_vars)
    |> tap (fun p ->
           log ~level:Logger.DEBUG "unfold" (fun () ->
               [ ("WITH_UPDATE_VARS", polyh_to_guard am p |> Guard.to_string ~pretty:true) ]))
    |> meet am update_guard
    |> tap (fun p ->
           log ~level:Logger.DEBUG "unfold" (fun () ->
               [
                 ("INITIAL_POLYH", polyh_to_guard am initial_polyh |> Guard.to_string ~pretty:true);
                 ("WITH_UPDATE_GUARD", polyh_to_guard am p |> Guard.to_string ~pretty:true);
               ]))
    |> update_polyh am update_approx
    |> tap (fun p ->
           log ~level:Logger.DEBUG "unfold" (fun () ->
               [
                 ( "UPDATE",
                   Map.to_sequence update_approx
                   |> Util.sequence_to_string ~f:(fun (x, p) ->
                          Printf.sprintf "%s = %s" (Var.to_string ~pretty:true x)
                            (Polynomials.Polynomial.to_string_pretty p)) );
                 ("WITH_UPDATE", polyh_to_guard am p |> Guard.to_string ~pretty:true);
               ]))
    |> project_polyh am program_vars
    |> tap (fun p ->
           log ~level:Logger.DEBUG "unfold" (fun () ->
               [
                 ("PROGRAM_VARS", VarSet.to_string program_vars);
                 ("PROJECTED", polyh_to_guard am p |> Guard.to_string ~pretty:true);
               ]))
    |> polyh_to_guard am
end

(** Generic type for abstractions *)
module type Abstraction = sig
  type context
  (** Context of the abstraction, for example properties *)

  type guard = Constraints.Constraint.t
  (** Constraint type *)

  type location

  type abstracted [@@derive eq, ord]
  (** The type to which the constraint is abstracted to *)

  type t = Abstr of abstracted | Constr of guard [@@deriving eq, ord]

  val abstract : context -> location -> guard -> t option
  (** Abstract a constraint to the abstracted type. Can return None, when the
 constraint is not satisfiable *)

  val to_string : ?pretty:bool -> t -> string
  val to_guard : t -> guard
end

module PropertyBasedAbstraction
    (PM : ProgramTypes.ProgramModules)
    (Adapter : Adapter
                 with type update_element = PM.UpdateElement.t
                  and type transition_label = PM.TransitionLabel.t) : sig
  include Abstraction with type location = PM.Location.t

  val mk_from_heuristic_scc : config -> PM.TransitionGraph.t -> PM.TransitionSet.t -> VarSet.t -> context
end = struct
  open PM
  open ApronUtils (PM)
  open OverApproximationUtils (Adapter)
  open Formulas
  open Constraints
  open Atoms

  module AtomSet = struct
    include MakeSetCreators0 (Atom)

    let to_string ?(pretty = false) t =
      Set.to_sequence t |> Util.sequence_to_string ~f:(Atom.to_string ~pretty)


    let compare = Set.compare_direct
    let equal s1 s2 = compare s1 s2 == 0
  end

  module LocationMap = MakeMapCreators1 (PM.Location)
  module Loops = Loops (PM)
  module FVS = FVS (PM)

  type context = AtomSet.t LocationMap.t
  (** Contains a set of properties for every location that should be
      abstracted, every missing location shall not be abstracted *)

  type guard = Constraint.t [@@deriving eq, ord]
  type location = Location.t

  type abstracted = AtomSet.t [@@deriving eq, ord]
  (** The abstracted type guard type *)

  (** Marks if the guard was abstracted or not *)
  type t = Abstr of abstracted | Constr of guard [@@deriving eq, ord]

  (** Properties are a set of constraints. If φ is in the properties, then it's
      negation ¬φ should not, because it is already checked as well. Adding it
      though would just result in double checks (overhead) *)
  (* let mk_properties props_list = *)
  (*   List.fold *)
  (*     (fun props prop -> *)
  (*       (1* Add property only, if its negation is not already present *1) *)
  (*       if not (AtomSet.mem (Atom.neg prop) props) then AtomSet.add prop props *)
  (*       else props) *)
  (*     AtomSet.empty props_list *)

  let mk_from_heuristic_scc config graph scc program_variables =
    let am = Ppl.manager_alloc_loose () in

    (* TODO eliminate non contributing variables *)
    let pr_h outgoing_transition =
      outgoing_transition |> PM.Transition.label |> PM.TransitionLabel.guard
      |> project_guard am program_variables |> AtomSet.of_list
    in

    let pr_hv outgoing_transition =
      outgoing_transition |> PM.Transition.label |> PM.TransitionLabel.guard
      |> bound_variables_guard am program_variables
      |> project_guard am program_variables |> AtomSet.of_list
    in

    let pr_c incoming_transition =
      let label = PM.Transition.label incoming_transition in
      let update_approx, guard_approx = PM.TransitionLabel.update_map label |> overapprox_update in
      let guard = PM.TransitionLabel.guard label |> Guard.mk_and guard_approx in
      update_guard am update_approx guard |> project_guard am program_variables |> AtomSet.of_list
    in

    let pr_cv incoming_transition =
      let label = PM.Transition.label incoming_transition in
      let update_approx, guard_approx = PM.TransitionLabel.update_map label |> overapprox_update in
      let guard = PM.TransitionLabel.guard label |> Guard.mk_and guard_approx in
      guard_to_polyh am guard |> project_polyh am program_variables
      |> bound_variables_polyh am program_variables
      |> AtomSet.of_list
    in

    let pr_d (loop : Transition.t list) =
      (* Backward propagation captures the properties needed for a full loop *)
      List.fold_right
        ~f:(fun transition combined ->
          log ~level:Logger.DEBUG "heuristic.backprop" (fun () ->
              [
                ("TRANSITION", Transition.to_string transition);
                ("PROPS", Guard.to_string ~pretty:true combined);
              ]);
          let label = PM.Transition.label transition in
          let update_approx, update_guard = PM.TransitionLabel.update_map label |> overapprox_update
          and constr = PM.TransitionLabel.guard label in
          Constraint.Infix.(
            constr && update_guard
            && Constraint.map_polynomial (Polynomials.Polynomial.substitute_all update_approx) combined)
          |> project_guard am program_variables)
        loop ~init:Constraint.mk_true
      |> AtomSet.of_list
    in

    let deduplicate_props props =
      let rec dedup selected = function
        | [] -> selected
        | prop :: props ->
            if
              List.find
                ~f:(fun other_prop ->
                  SMT.Z3Solver.equivalent
                    (Formula.mk (Guard.mk [ prop ]))
                    (Formula.mk (Guard.mk [ other_prop ])))
                selected
              |> Option.is_some
            then
              dedup selected props
            else
              dedup (prop :: selected) props
      in

      dedup [] (Set.to_list props) |> AtomSet.of_list
    in

    let for_transitions f transitions =
      List.fold ~f:(fun props t -> f t |> Set.union props) ~init:AtomSet.empty transitions
    in

    let for_loops f loops =
      List.fold ~f:(fun props loop -> f loop |> Set.union props) ~init:AtomSet.empty loops
    in

    let log_props location name props =
      log ~level:Logger.DEBUG "heuristic" (fun () ->
          [ ("LOCATION", Location.to_string location); (name, AtomSet.to_string ~pretty:true props) ]);
      props
    in

    let loops = Loops.find_loops_scc graph scc in

    (* The locations to abstract *)
    let heads =
      match config.abstract with
      | `FVS -> FVS.fvs ~loops:(Some loops) graph
      | `LoopHeads -> Loops.loop_heads loops
    in

    Set.fold
      ~f:(fun properties head ->
        let outgoing_transitions = TransitionGraph.succ_e graph head in
        let incoming_transitions = TransitionGraph.pred_e graph head in
        (* Find loops containing the head, and rotate *)
        let loops_with_head =
          loops
          |> List.filter ~f:(fun l -> List.mem ~equal:Location.equal l head)
          |> List.map ~f:(Loops.rotate head)
        in
        log ~level:Logger.DEBUG "heuristic" (fun () ->
            [
              ("LOCATION", Location.to_string head); ("ROTATED_LOOPS", Loops.loops_to_string loops_with_head);
            ]);
        let properties_for_head =
          AtomSet.empty
          |> Set.union (for_transitions pr_h outgoing_transitions |> log_props head "PR_h")
          |> Set.union (for_transitions pr_hv outgoing_transitions |> log_props head "PR_hv")
          |> Set.union (for_transitions pr_c incoming_transitions |> log_props head "PR_c")
          |> Set.union (for_transitions pr_cv incoming_transitions |> log_props head "PR_cv")
          |> Set.union
               (Loops.transition_loops_from graph loops_with_head |> for_loops pr_d |> log_props head "PR_d")
          |> deduplicate_props
          |> tap (fun props ->
                 log "heuristic" (fun () ->
                     [
                       ("LOCATION", Location.to_string head);
                       ("PROPERTIES", AtomSet.to_string ~pretty:true props);
                     ]))
        in
        Map.add_exn ~key:head ~data:properties_for_head properties)
      heads ~init:LocationMap.empty


  let abstract context location guard =
    (* Identifies all properties that are entailed by the constraint, implicit SAT check *)
    let abstract_guard_ properties constr =
      SMT.IncrementalZ3Solver.(
        let solver = create ~model:false () in
        let f = Formula.mk constr in
        add solver f;

        (* Fast entailment check which reuses the SMT solver *)
        let entails_prop prop =
          let neg_atom = Atom.neg prop |> Constraint.lift |> Formula.mk in
          push solver;
          add solver neg_atom;
          let is_unsat = unsatisfiable solver in
          pop solver;
          is_unsat
        in

        (* No need to check entailment, when constraint is already UNSAT *)
        if unsatisfiable solver then
          None
          (* Check entailment for every propery it's negation *)
        else
          let entailed_props =
            Set.to_sequence properties
            |> Sequence.filter_map ~f:(fun prop ->
                   if entails_prop prop then (
                     log ~level:Logger.DEBUG "abstract" (fun () -> [ ("ENTAILS", Atom.to_string prop) ]);
                     (* (Constraint.to_string constr) *)
                     (* (Atom.to_string prop); *)
                     Some prop)
                   else if entails_prop (Atom.neg prop) then (
                     log ~level:Logger.DEBUG "abstract" (fun () -> [ ("ENTAILS_NEG", Atom.to_string prop) ]);
                     (* Printf.printf "%s entails %s\n" *)
                     (* (Constraint.to_string constr) *)
                     (* (Atom.to_string (Atom.neg prop)); *)
                     Some (Atom.neg prop))
                   else
                     None)
            |> AtomSet.of_sequence
          in
          Some entailed_props)
    in

    match Map.find context location with
    | Some properties ->
        log ~level:Logger.DEBUG "abstract" (fun () ->
            [
              ("LOCATION", Location.to_string location);
              ("PROPERTIES", AtomSet.to_string ~pretty:true properties);
            ]);
        abstract_guard_ properties guard |> Option.map ~f:(fun a -> Abstr a)
    | None ->
        log ~level:Logger.DEBUG "abstract" (fun () ->
            [ ("LOCATION", Location.to_string location); ("PROPERTIES", "NONE") ]);
        if Formula.mk guard |> SMT.Z3Solver.satisfiable then
          Some (Constr guard)
        else
          None


  let abstract_to_guard a = Set.to_list a |> Constraint.mk

  let to_guard a =
    match a with
    | Abstr abstracted -> abstract_to_guard abstracted
    | Constr guard -> guard


  let to_string ?(pretty = false) = function
    | Abstr a -> Printf.sprintf "Abstracted [%s]" (Constraint.to_string ~pretty (abstract_to_guard a))
    | Constr c -> Printf.sprintf "Constraints [%s]" (Constraint.to_string ~pretty c)
end

(** A version is a location with an (possibly abstracted) constraint used
  in the partial evaluation graph *)
module Version (L : ProgramTypes.Location) (A : Abstraction) = struct
  module Inner = struct
    open Constraints

    type location = L.t
    type t = L.t * A.t [@@deriving eq, ord]

    let sexp_of_t = Sexplib0.Sexp_conv.sexp_of_opaque
    let to_string (l, a) = Printf.sprintf "⟨%s, %s⟩" (L.to_string l) (A.to_string ~pretty:false a)
    let to_string_pretty (l, a) = Printf.sprintf "⟨%s, %s⟩" (L.to_string l) (A.to_string ~pretty:true a)
    let hash l = Hashtbl.hash l
    let mk location abstracted = (location, abstracted)
    let mk_true location = (location, A.Constr Constraint.mk_true)

    (* TODO: this is broken, because it doesn't parse the guard, but this isn't used anyway and only
       here for interface compatibility *)
    let of_string s = mk_true (L.of_string s)
    let location (l, _) = l
    let abstracted (_, a) = a
  end

  include Inner
  include Comparator.Make (Inner)
end

(* TODO: move somewhere else *)
module GraphUtils (PM : ProgramTypes.ProgramModules) = struct
  open PM

  module TransitionGraphWeight (Value : PolyTypes.Ring) = struct
    type t = Value.t
    type edge = PM.TransitionGraph.E.t

    let weight (x : edge) = Value.one
    let compare x y = 0
    let add x y = Value.add x y
    let zero = Value.zero
  end

  module Djikstra = Graph.Path.Dijkstra (PM.TransitionGraph) (TransitionGraphWeight (OurInt))

  let entry_transitions graph scc =
    TransitionSet.locations scc |> Set.to_sequence
    |> Sequence.concat_map ~f:(fun l -> TransitionGraph.pred_e graph l |> Sequence.of_list)
    |> Sequence.filter ~f:(fun t -> not (Set.mem scc t))
    |> TransitionSet.of_sequence


  let exit_transitions graph scc =
    TransitionSet.locations scc |> Set.to_sequence
    |> Sequence.concat_map ~f:(fun l -> TransitionGraph.succ_e graph l |> Sequence.of_list)
    |> Sequence.filter ~f:(fun t -> not (Set.mem scc t))
    |> TransitionSet.of_sequence
end

module PEModules
    (PM : ProgramTypes.ProgramModules)
    (Adapter : Adapter with type location = PM.Location.t)
    (A : Abstraction) =
struct
  module Version = Version (PM.Location) (A)
  module VersionSet = Location.LocationSetOver (Version)
  module Transition = Transition_.Make (PM.TransitionLabel) (Version)
  module TransitionSet = Transition_.TransitionSetOver (Transition) (Version)

  module TransitionGraph =
    TransitionGraph_.Make_ (Transition) (Version)
      (Graph.Persistent.Digraph.ConcreteBidirectionalLabeled (Version) (PM.TransitionLabel))

  let rename_versions (versions : TransitionGraph.location_set) =
    (* There is no nice way of renaming locations.
       1. We reuse the original name, if the location is unique in the pe_graph,
          that happens when the location was not part of the scc.
       2. We add a suffix _i if the location was multiplied (part of the scc), and l_i
          is not already present
       3. Increment i until a unique name is found.

       There is certainly a better solution to this problem, but this will work
       for now.

       TODO: rewrite, and probably move to Location module.
    *)
    let num_versions = Set.length versions in
    let num_of_location_occurences = Hashtbl.create ~size:num_versions (module PM.Location) in
    Set.iter
      ~f:(fun version ->
        let location = Version.location version in
        let num = Hashtbl.find num_of_location_occurences location |? 0 |> ( + ) 1 in
        Hashtbl.update num_of_location_occurences location ~f:(fun _ -> num))
      versions;

    let suffix_counters = Hashtbl.create ~size:num_versions (module PM.Location)
    and rename_map = Hashtbl.create ~size:num_versions (module Version) in
    Set.iter
      ~f:(fun version ->
        let location = Version.location version in
        let num_occ = Hashtbl.find_exn num_of_location_occurences location in
        assert (num_occ >= 1);
        if num_occ == 1 then
          (* reuse location *)
          Hashtbl.add_exn rename_map ~key:version ~data:location
        else
          (* generate a fresh suffix *)
          let rec gen_new_location () =
            let next_index = Hashtbl.find suffix_counters location |> Option.value ~default:0 in
            Hashtbl.update suffix_counters location ~f:(fun _ -> next_index + 1);
            let potential_new_location = Adapter.location_with_index next_index location in
            if Hashtbl.mem num_of_location_occurences potential_new_location then
              gen_new_location ()
            else
              potential_new_location
          in

          Hashtbl.add_exn rename_map ~key:version ~data:(gen_new_location ()))
      versions;
    rename_map


  let remove_abstractions update_invariants pe_graph =
    let rename_map = rename_versions (TransitionGraph.locations pe_graph) in
    let renamed_graph =
      TransitionGraph.transitions pe_graph |> Set.to_sequence
      |> Sequence.map ~f:(fun (src_version, label, target_version) ->
             (Hashtbl.find_exn rename_map src_version, label, Hashtbl.find_exn rename_map target_version))
      |> PM.TransitionGraph.mk
    in
    if update_invariants then
      TransitionGraph.locations pe_graph
      |> Set.fold
           ~f:(fun graph version ->
             let renamed_loc = Hashtbl.find_exn rename_map version in
             let inv = A.to_guard (Version.abstracted version) in
             PM.TransitionGraph.add_invariant renamed_loc inv graph)
           ~init:renamed_graph
    else
      renamed_graph
end

module PartialEvaluation
    (PM : ProgramTypes.ProgramModules)
    (Adapter : Adapter
                 with type update_element = PM.UpdateElement.t
                  and type transition_label = PM.TransitionLabel.t
                  and type program = PM.Program.t
                  and type location = PM.Location.t
                  and type transition_graph = PM.TransitionGraph.t) =
struct
  module PM = PM
  module Abstraction = PropertyBasedAbstraction (PM) (Adapter)
  module PEM = PEModules (PM) (Adapter) (Abstraction)
  open PM

  (* open Adapter *)
  module Version = PEM.Version
  module PEGraph = PEM.TransitionGraph
  open Unfolding (PM) (Adapter)
  open Loops (PM)
  open GraphUtils (PM)

  exception UnsatEntryTransition
  (** Unsat transitions must be removed before the partial evaluation, hence
      unfolded entry transition to an scc <l, true> -> <l', v'> must always
      have a satisfiable version v'
      *)

  let am = Ppl.manager_alloc_loose ()

  (* TODO: find a better name, component is wrong, it's just a set of transitions *)
  let evaluate_component config component program_vars start_location graph =
    let entry_transitions = entry_transitions graph component
    and exit_transitions = exit_transitions graph component in

    log "pe" (fun () -> [ ("EVALUATING_SCC", TransitionSet.to_string component) ]);
    log "pe" (fun () -> [ ("ENTRY_TS:", TransitionSet.to_string entry_transitions) ]);
    log "pe" (fun () -> [ ("EXIT_TS:", TransitionSet.to_string exit_transitions) ]);

    let abstr_ctx = Abstraction.mk_from_heuristic_scc config graph component program_vars in
    let result_graph_without_sub =
      TransitionGraph.transitions graph |> Set.to_sequence
      |> Sequence.filter ~f:(fun transition -> not (Set.mem component transition))
      (* Exit transitions will be readded by the evaluation *)
      |> Sequence.filter ~f:(fun transition -> not (Set.mem exit_transitions transition))
      |> Sequence.map ~f:(fun (src, label, target) ->
             (PEM.Version.mk_true src, label, Version.mk_true target))
      |> PEGraph.mk
    in

    let rec evaluate_version current_version (already_unfolded_versions, result_graph) =
      let evaluate_transition src_version (already_unfolded_versions, result_graph) (transition, is_exit) =
        assert (Transition.src transition == Version.location src_version);
        (* Unwrap the transition *)
        let src_loc, label, target_loc = transition in
        let src_constr = current_version |> Version.abstracted |> Abstraction.to_guard in
        let guard = TransitionLabel.guard label and update = TransitionLabel.update_map label in

        (* stop early if guard is unsat *)
        if SMT.Z3Solver.unsatisfiable (Formulas.Formula.lift [ src_constr; guard ]) then
          (already_unfolded_versions, result_graph)
        else
          let next_guard = unfold am src_constr program_vars guard update in
          log ~level:DEBUG "pe.transition" (fun () ->
              [
                ("PREV_GUARD", Guard.to_string ~pretty:true guard);
                ("TRANSITION", Transition.to_string_pretty transition);
                ("NEXT_GUARD", Guard.to_string ~pretty:true next_guard);
              ]);
          match Abstraction.abstract abstr_ctx target_loc next_guard with
          (* Implicit SAT check, returns None if UNSAT *)
          | Some a ->
              log ~level:DEBUG "pe.transition" (fun () ->
                  [ ("SAT", "true"); ("ABSTRACTED", Abstraction.to_string ~pretty:true a) ]);
              (* checking for exti transitions must be done outside of evaluate_transition because
                 the transition was renamed *)
              if is_exit then
                (* Leaving the scc is an abort condition, just transition to the trivial version *)
                (* let outside_version = Version.mk_true target_loc in *)
                (* assert (PEGraph.mem_vertex result_graph outside_version); *)
                ( already_unfolded_versions,
                  PEGraph.add_edge_e result_graph (src_version, label, Version.mk_true target_loc) )
              else
                let target_version = Version.mk target_loc a in
                let new_transition = (src_version, label, target_version) in
                log ~level:DEBUG "pe.transition" (fun () ->
                    [ ("NEW_TRANSITION", PEM.Transition.to_string_pretty new_transition) ]);
                let result_graph = PEGraph.add_edge_e result_graph new_transition in
                evaluate_version target_version (already_unfolded_versions, result_graph)
          | None ->
              log ~level:DEBUG "pe.transition" (fun () -> [ ("VERSION_SAT", "false") ]);
              (* Target version is unsat take, backtrack without adding a new transition *)
              (already_unfolded_versions, result_graph)
      in

      if Set.mem already_unfolded_versions current_version then (
        log ~level:DEBUG "pe.version" (fun () ->
            [ ("VERSION", Version.to_string_pretty current_version); ("NEW", "false") ]);
        (already_unfolded_versions, result_graph))
      else (
        log ~level:DEBUG "pe.version" (fun () ->
            [ ("VERSION", Version.to_string_pretty current_version); ("NEW", "true") ]);
        let already_unfolded_versions = Set.add already_unfolded_versions current_version in
        let location = Version.location current_version in

        let gt_id_map = Hashtbl.create ~size:10 (module Int) in
        let outgoing_transitions =
          TransitionGraph.succ_e graph location
          |> Sequence.of_list
          |> Sequence.map ~f:(fun transition -> (transition, Set.mem exit_transitions transition))
          |> Sequence.map ~f:(fun ((src, label, target), is_exit) ->
                 (* ((src, TransitionLabel.copy_rename gt_id_map label, target), is_exit)) *)
                 ((src, TransitionLabel.copy_rename gt_id_map label, target), is_exit))
          |> Sequence.map
               ~f:
                 (tap (fun (t, is_exit) ->
                      log ~level:DEBUG "pe.version" (fun () ->
                          [
                            ("RENAMED_TRANSITION", Transition.to_string t); ("IS_EXIT", Bool.to_string is_exit);
                          ])))
          |> Sequence.to_list
        in

        (* Evaluate from every outgoing transition *)
        List.fold
          ~f:(evaluate_transition current_version)
          ~init:(already_unfolded_versions, result_graph)
          outgoing_transitions)
    in

    let entry_versions =
      Set.fold
        ~f:(fun entry_versions entry_transition ->
          let entry_location = Transition.target entry_transition in
          let entry_version = Version.mk_true entry_location in
          Set.add entry_versions entry_version)
        entry_transitions ~init:PEM.VersionSet.empty
    in

    let entry_versions =
      if Set.mem (TransitionSet.locations component) start_location then
        Set.add entry_versions (Version.mk_true start_location)
      else
        entry_versions
    in

    let _, pe_graph =
      Set.fold
        ~f:(fun (already_unfolded_versions, result_graph) entry_version ->
          evaluate_version entry_version (already_unfolded_versions, result_graph))
        entry_versions
        ~init:(PEM.VersionSet.empty, result_graph_without_sub)
    in

    let pe_graph = PEM.remove_abstractions config.update_invariants pe_graph in
    pe_graph


  let evaluate_program config program =
    let program_vars =
      Program.input_vars program
      |> tap (fun x -> log "pe" (fun () -> [ ("PROGRAM_VARS", VarSet.to_string x) ]))
    in
    let orig_graph = Program.graph program in
    let pe_graph =
      evaluate_component config
        (TransitionGraph.transitions orig_graph)
        program_vars (Program.start program) orig_graph
    in
    let pe_prog =
      (* Note: the start location cannot be part of an SCC and hence is
         Also present in the result graph.
      *)
      Adapter.create_new_program (Program.start program) pe_graph
    in
    assert (VarSet.equal (Program.input_vars program) (Program.input_vars pe_prog));
    pe_prog


  let apply_sub_scc_cfr config (nonLinearTransitions : TransitionSet.t) program =
    let program_vars =
      Program.input_vars program
      |> tap (fun x -> log "pe" (fun () -> [ ("PROGRAM_VARS", VarSet.to_string x) ]))
    in
    let orig_graph = Program.graph program in

    let pe_graph =
      let find_smallest_loop transition =
        let src, _, target = transition in
        let shortest_path, _length = Djikstra.shortest_path orig_graph target src in
        transition :: shortest_path |> TransitionSet.of_list
      in

      let find_parallel_transitions transition =
        let src, _, target = transition in
        TransitionGraph.find_all_edges orig_graph src target |> TransitionSet.of_list
      in

      let component =
        nonLinearTransitions
        |> Set.fold
             ~f:(fun loops transition -> Set.union loops (find_smallest_loop transition))
             ~init:TransitionSet.empty
        |> Set.fold
             ~f:(fun parallel transition -> Set.union parallel (find_parallel_transitions transition))
             ~init:TransitionSet.empty
      in
      evaluate_component config component program_vars (Program.start program) orig_graph
    in
    let pe_prog = Adapter.create_new_program (Program.start program) pe_graph in
    log ~level:INFO "pe" (fun () -> [ ("PE", Program.to_string pe_prog) ]);
    pe_prog
end

module ClassicPartialEvaluation = PartialEvaluation (ProgramModules) (ClassicAdapter)
module ProbabilisticPartialEvaluation = PartialEvaluation (ProbabilisticProgramModules) (ProbabilisticAdapter)
