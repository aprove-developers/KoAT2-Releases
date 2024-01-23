open! OurBase

let cfr_logger = Logging.(get CFR)

let log ?(level = Logger.INFO) method_name data =
  Logger.log cfr_logger level (fun () -> (method_name, data ()))


(* TODO: Is k_encounters used somewhere *)
type config = { abstract : [ `FVS | `LoopHeads ]; k_encounters : int; update_invariants : bool }

module ApronUtils = struct
  open Constraints
  open Polynomials

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
  let update_to_apron env (update : Polynomials.Polynomial.t ProgramTypes.VarMap.t) =
    Map.to_sequence update
    |> Sequence.map ~f:(fun (var, ue) ->
           let apron_var = ApronInterface.Koat2Apron.var_to_apron var
           and apron_expr = ApronInterface.Koat2Apron.poly_to_apron env ue in
           (apron_var, apron_expr))
    |> Sequence.to_array |> Array.unzip


  (** GenericProgram_.Adapter for polyheron operations used directly on constraints *)
  let guard_to_polyh am guard =
    let env = Apron.Environment.make (ApronInterface.Koat2Apron.vars_to_apron (Constraint.vars guard)) [||] in
    let tcons_array = ApronInterface.Koat2Apron.constraint_to_apron env guard in
    let polyh = Apron.Abstract1.of_tcons_array am env tcons_array in
    polyh


  (** GenericProgram_.Adapter for polyheron operations used directly on constraints *)
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

module OverApproximationUtils (A : GenericProgram_.Adapter) = struct
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
    (A : GenericProgram_.Adapter
           with type update_element = PM.UpdateElement.t
            and type transition = PM.Transition.t) =
struct
  open OverApproximationUtils (A)
  open ApronUtils

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

  type abstracted [@@derive eq, ord]
  (** The type to which the constraint is abstracted to *)

  type t = Abstr of abstracted | Constr of guard [@@deriving eq, ord]

  val is_true : t -> bool

  val abstract : context -> Location.t -> guard -> t
  (** Abstract a constraint to the abstracted type. *)

  val to_string : ?pretty:bool -> t -> string
  val to_guard : t -> guard
end

module PropertyBasedAbstraction
    (PM : ProgramTypes.ProgramModules)
    (Adapter : GenericProgram_.Adapter
                 with type update_element = PM.UpdateElement.t
                  and type transition = PM.Transition.t) : sig
  include Abstraction

  val mk_from_heuristic_scc : config -> PM.TransitionGraph.t -> PM.TransitionSet.t -> VarSet.t -> context
end = struct
  open PM
  open ApronUtils
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

  module LocationMap = MakeMapCreators1 (Location)
  open Cycles
  module Cycles = Cycles (PM)
  module FVS = FVS.FVS (PM)

  type context = AtomSet.t LocationMap.t
  (** Contains a set of properties for every location that should be
      abstracted, every missing location shall not be abstracted *)

  type guard = Constraint.t [@@deriving eq, ord]

  type abstracted = AtomSet.t [@@deriving eq, ord]
  (** The abstracted type guard type *)

  (** Marks if the guard was abstracted or not *)
  type t = Abstr of abstracted | Constr of guard [@@deriving eq, ord]

  let is_true = function
    | Constr guard -> Guard.is_true guard
    | Abstr a -> Set.is_empty a


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

    let pr_d (cycle : Transition.t list) =
      (* Backward propagation captures the properties needed for a full cycle *)
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
        cycle ~init:Constraint.mk_true
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

    let for_cycles f cycles =
      List.fold ~f:(fun props cycle -> f cycle |> Set.union props) ~init:AtomSet.empty cycles
    in

    let log_props location name props =
      log ~level:Logger.DEBUG "heuristic" (fun () ->
          [ ("LOCATION", Location.to_string location); (name, AtomSet.to_string ~pretty:true props) ]);
      props
    in

    let cycles = Cycles.find_cycles_scc graph scc in

    (* The locations to abstract *)
    let heads =
      match config.abstract with
      | `FVS -> FVS.fvs ~cycles:(Some cycles) graph
      | `LoopHeads -> Cycles.loop_heads graph cycles
    in

    Set.fold
      ~f:(fun properties head ->
        let outgoing_transitions = TransitionGraph.succ_e graph head in
        let incoming_transitions = TransitionGraph.pred_e graph head in
        (* Find cycles containing the head, and rotate *)
        let cycles_with_head =
          cycles
          |> List.filter ~f:(fun l -> List.mem ~equal:Location.equal l head)
          |> List.map ~f:(Cycles.rotate head)
        in
        log ~level:Logger.DEBUG "heuristic" (fun () ->
            [
              ("LOCATION", Location.to_string head);
              ("ROTATED_CYCLES", Cycles.cycles_to_string cycles_with_head);
            ]);
        let properties_for_head =
          AtomSet.empty
          |> Set.union (for_transitions pr_h outgoing_transitions |> log_props head "PR_h")
          |> Set.union (for_transitions pr_hv outgoing_transitions |> log_props head "PR_hv")
          |> Set.union (for_transitions pr_c incoming_transitions |> log_props head "PR_c")
          |> Set.union (for_transitions pr_cv incoming_transitions |> log_props head "PR_cv")
          |> Set.union
               (Cycles.transition_cycles_from graph cycles_with_head
               |> for_cycles pr_d |> log_props head "PR_d")
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
      let module Solver = SMT.IncrementalZ3Solver in
      let solver = Solver.create ~model:false () in
      let f = (* TODO linearize constraints? *) Formula.mk constr in
      Solver.add solver f;

      (* Fast entailment check which reuses the SMT solver *)
      let entails_prop prop =
        let neg_atom = Atom.neg prop |> Constraint.lift |> Formula.mk in
        Solver.push solver;
        Solver.add solver neg_atom;
        let is_unsat = Solver.unsatisfiable solver in
        Solver.pop solver;
        is_unsat
      in

      let entailed_props =
        Set.to_sequence properties
        |> Sequence.filter_map ~f:(fun prop ->
               if entails_prop prop then (
                 log ~level:Logger.DEBUG "abstract" (fun () -> [ ("ENTAILS", Atom.to_string prop) ]);
                 Some prop)
               else if entails_prop (Atom.neg prop) then (
                 log ~level:Logger.DEBUG "abstract" (fun () -> [ ("ENTAILS_NEG", Atom.to_string prop) ]);
                 Some (Atom.neg prop))
               else
                 None)
        |> AtomSet.of_sequence
      in
      entailed_props
    in

    match Map.find context location with
    | Some properties ->
        log ~level:Logger.DEBUG "abstract" (fun () ->
            [
              ("LOCATION", Location.to_string location);
              ("PROPERTIES", AtomSet.to_string ~pretty:true properties);
            ]);
        Abstr (abstract_guard_ properties guard)
    | None ->
        log ~level:Logger.DEBUG "abstract" (fun () ->
            [ ("LOCATION", Location.to_string location); ("PROPERTIES", "NONE") ]);
        Constr guard


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
module Version (A : Abstraction) = struct
  module Inner = struct
    open Constraints

    type t = Location.t * A.t [@@deriving ord]

    let sexp_of_t = Sexplib0.Sexp_conv.sexp_of_opaque
    let _to_string (l, a) = Printf.sprintf "⟨%s, %s⟩" (Location.to_string l) (A.to_string ~pretty:false a)

    let to_string_pretty (l, a) =
      Printf.sprintf "⟨%s, %s⟩" (Location.to_string l) (A.to_string ~pretty:true a)


    let hash l = Hashtbl.hash l
    let mk location abstracted = (location, abstracted)
    let mk_true location = (location, A.Constr Constraint.mk_true)
    let location (l, _) = l
    let abstracted (_, a) = a
    let is_true (_, a) = A.is_true a
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

module PartialEvaluation
    (PM : ProgramTypes.ProgramModules)
    (Adapter : GenericProgram_.Adapter
                 with type update_element = PM.UpdateElement.t
                  and type transition = PM.Transition.t
                  and type program = PM.Program.t
                  and type transition_graph = PM.TransitionGraph.t) =
struct
  module Abstraction = PropertyBasedAbstraction (PM) (Adapter)
  module Version = Version (Abstraction)
  module VersionSet = MakeSetCreators0 (Version)
  open PM
  open Unfolding (PM) (Adapter)
  open Cycles
  open Cycles (PM)
  open GraphUtils (PM)

  (** The call [generate_version_location_name program_locations index_table version] generates a new location name by appending the string [_vi] to the location of [version] where [i] acts as an index.
      The hashtable [index_table] maps locations to the last {i used} index.

     Currently, there is no nice way of renaming locations.
     1. We reuse the original name, if the location is unique in the pe_graph,
        that happens when the location was not part of the scc.
     2. We add a suffix _vi (where i acts as an index) if the location was multiplied (part of the scc), and l_i
        is not already present
     3. Increment i until a unique name is found.

     There is certainly a better solution to this problem, but this will work
     for now.

     TODO: rewrite, and probably move to Location module.
  *)
  let generate_version_location_name all_original_locations index_table version =
    let location = Version.location version in
    let rec get_next_location () =
      let next_index =
        Hashtbl.update_and_return index_table location ~f:(Option.value_map ~default:1 ~f:(( + ) 1))
      in
      let next_location =
        Printf.sprintf "%s_v%i" (Location.to_string location) next_index |> Location.of_string
      in
      if Set.mem all_original_locations next_location then
        get_next_location ()
      else
        next_location
    in

    if Version.is_true version then
      location
    else
      get_next_location ()


  let evaluate_component config component program_vars program_start graph =
    let am = Ppl.manager_alloc_loose () in

    let entry_transitions = entry_transitions graph component
    and exit_transitions = exit_transitions graph component in
    let entry_versions =
      let from_entry_trans =
        Set.fold
          ~f:(fun entry_versions entry_transition ->
            let entry_location = Transition.target entry_transition in
            Set.add entry_versions (Version.mk_true entry_location))
          entry_transitions ~init:VersionSet.empty
      in
      if Set.mem (TransitionSet.locations component) program_start then
        Set.add from_entry_trans (Version.mk_true program_start)
      else
        from_entry_trans
    in

    let generate_version_location_name =
      let program_locations = TransitionGraph.locations graph in
      let index_table = Hashtbl.create (module Location) in
      fun version -> generate_version_location_name program_locations index_table version
    in
    let version_location_tbl = Hashtbl.create (module Version) in

    let abstr_ctx = Abstraction.mk_from_heuristic_scc config graph component program_vars in

    let evaluate_version current_version =
      let evaluate_transition src_polyh transition =
        let src_loc, label, target_loc = transition in
        assert (Location.equal src_loc (Version.location current_version));
        if Set.mem exit_transitions transition then
          `ExitTransition (Version.mk_true target_loc)
        else
          let update = TransitionLabel.update_map label in
          let unfolded_constr = unfold_update am src_polyh program_vars update in
          let abstracted = Abstraction.abstract abstr_ctx target_loc unfolded_constr in
          `EvaluatedTransition (Version.mk target_loc abstracted)
      in
      let evaluate_grouped_transition grouped_transition :
          (Adapter.grouped_transition * Version.t List.t) Option.t =
        let src_constr = current_version |> Version.abstracted |> Abstraction.to_guard in
        let guard = Adapter.guard_of_grouped_transition grouped_transition in
        let src_polyh = initial_guard_polyh am src_constr guard in
        if Apron.Abstract1.is_bottom am src_polyh then
          (* Version + Transition guard is UNSAT *)
          None
        else
          let version_start_loc =
            Hashtbl.find_or_add version_location_tbl current_version ~default:(fun () ->
                generate_version_location_name current_version)
          in
          let next_versions = ref [] in
          let evaluated_grouped_transition =
            grouped_transition
            |> Adapter.copy_and_modify_grouped_transition ~new_start:version_start_loc
                 ~add_invariant:src_constr ~redirect:(fun trans ->
                   match evaluate_transition src_polyh trans with
                   | `ExitTransition target_version ->
                       (* Here, we exit the component hence we go to the original location *)
                       Version.location target_version
                   | `EvaluatedTransition next_version -> (
                       match Hashtbl.find version_location_tbl next_version with
                       | Some target_location ->
                           (* We have already seen next_version *)
                           target_location
                       | None ->
                           let target_location = generate_version_location_name next_version in
                           Hashtbl.add_exn version_location_tbl ~key:next_version ~data:target_location;
                           next_versions := next_version :: !next_versions;
                           target_location))
          in
          Some (evaluated_grouped_transition, !next_versions)
      in
      let refined_outgoing_grouped_transitions, next_versionss =
        Adapter.outgoing_grouped_transitions graph (Version.location current_version)
        |> Sequence.map ~f:evaluate_grouped_transition
        |> Sequence.filter_opt |> Sequence.to_list |> List.unzip
      in
      (refined_outgoing_grouped_transitions, List.concat next_versionss)
    in

    let version_stack_to_string =
      Util.sequence_to_string ~f:Version.to_string_pretty % Sequence.of_list % Stack.to_list
    in

    let evaluate_versions_till_fixedpoint remaining_versions =
      let rec evaluate_ refined_grouped_transitions already_evaluated_versions =
        match Stack.pop remaining_versions with
        | None -> refined_grouped_transitions
        | Some next_version when Set.mem already_evaluated_versions next_version ->
            log "evaluate_versions_till_fixedpoint.already_evaluated" (fun () ->
                [
                  ("version", Version.to_string_pretty next_version);
                  ("remaining:", version_stack_to_string remaining_versions);
                ]);
            evaluate_ refined_grouped_transitions already_evaluated_versions
        | Some next_version ->
            let new_grouped_transitions, new_versions = evaluate_version next_version in
            log "evaluate_versions_till_fixedpoint" (fun () ->
                [
                  ("version", Version.to_string_pretty next_version);
                  ( "new_versions",
                    Util.sequence_to_string ~f:Version.to_string_pretty (Sequence.of_list new_versions) );
                  ("remaining:", version_stack_to_string remaining_versions);
                ]);

            let new_grouped_transitions =
              Set.of_list (Set.comparator_s Adapter.empty_grouped_transition_set) new_grouped_transitions
            in
            List.iter ~f:(fun version -> Stack.push remaining_versions version) new_versions;
            evaluate_
              (Set.union refined_grouped_transitions new_grouped_transitions)
              (Set.add already_evaluated_versions next_version)
      in
      evaluate_ Adapter.empty_grouped_transition_set VersionSet.empty
    in
    Stack.of_list (Set.to_list entry_versions)
    |> evaluate_versions_till_fixedpoint
    |> tap (fun _ ->
           log "evaluate_component" (fun () ->
               [
                 ( "version_location_tbl",
                   Hashtbl.to_alist version_location_tbl
                   |> List.map ~f:(fun (ver, loc) -> (loc, ver))
                   |> List.sort ~compare:(fun (loc1, _) (loc2, _) -> Location.compare loc1 loc2)
                   |> Sequence.of_list
                   |> Util.sequence_to_string ~f:(fun (loc, ver) ->
                          Location.to_string loc ^ ": " ^ Version.to_string_pretty ver) );
               ]))


  let evaluate_component_in_program config component program_vars program_start graph =
    let exec () =
      let all_grouped_transitions_without_component =
        let all_grouped_transitions = Adapter.all_grouped_transitions_of_graph graph in
        let all_grouped_transitions_in_component =
          Set.map
            (Set.comparator_s Adapter.empty_grouped_transition_set)
            ~f:Adapter.grouped_transition_of_transition component
        in
        Set.diff all_grouped_transitions all_grouped_transitions_in_component
      in
      let all_refined_grouped_transitions =
        evaluate_component config component program_vars program_start graph
      in
      Set.union all_grouped_transitions_without_component all_refined_grouped_transitions
      |> Adapter.create_new_program program_start
    in
    Logger.with_log cfr_logger Logger.INFO
      (fun () ->
        ( "evaluate_component_in_program",
          [
            ("component", TransitionSet.to_id_string_pretty component);
            ("program_vars", VarSet.to_string ~pretty:true program_vars);
          ] ))
      exec


  let evaluate_program config program =
    let program_vars =
      Program.input_vars program
      |> tap (fun x -> log "pe" (fun () -> [ ("PROGRAM_VARS", VarSet.to_string x) ]))
    in
    let pe_prog =
      evaluate_component_in_program config (Program.transitions program) program_vars (Program.start program)
        (Program.graph program)
    in

    assert (VarSet.equal (Program.input_vars program) (Program.input_vars pe_prog));
    pe_prog


  let apply_sub_scc_cfr config (non_linear_transitions : TransitionSet.t) program =
    let program_vars =
      Program.input_vars program
      |> tap (fun x -> log "pe" (fun () -> [ ("PROGRAM_VARS", VarSet.to_string x) ]))
    in
    let orig_graph = Program.graph program in

    let pe_prog =
      let find_smallest_cycle transition =
        let src, _, target = transition in
        let shortest_path, _length = Djikstra.shortest_path orig_graph target src in
        transition :: shortest_path |> TransitionSet.of_list
      in

      let find_parallel_transitions transition =
        let src, _, target = transition in
        TransitionGraph.find_all_edges orig_graph src target |> TransitionSet.of_list
      in

      let component =
        non_linear_transitions
        |> Set.fold
             ~f:(fun cycles transition -> Set.union cycles (find_smallest_cycle transition))
             ~init:TransitionSet.empty
        |> Set.fold
             ~f:(fun parallel transition -> Set.union parallel (find_parallel_transitions transition))
             ~init:TransitionSet.empty
      in
      evaluate_component_in_program config component program_vars (Program.start program) orig_graph
    in

    log ~level:INFO "pe" (fun () -> [ ("PE", Program.to_string pe_prog) ]);
    pe_prog
end

module ClassicPartialEvaluation = PartialEvaluation (ProgramModules) (ProgramModules.ClassicAdapter)

module ProbabilisticPartialEvaluation =
  PartialEvaluation (ProbabilisticProgramModules) (ProbabilisticProgramModules.ProbabilisticAdapter)
