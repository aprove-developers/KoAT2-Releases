open! OurBase

let cfr_logger = Logging.(get CFR)

type config = { abstract : [ `FVS | `LoopHeads ] }

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
    (A : GenericProgram_.Adapter
           with type update_element = PM.UpdateElement.t
            and type transition = PM.Transition.t) : sig
  include Abstraction

  val mk_from_heuristic_scc : config -> PM.TransitionGraph.t -> PM.TransitionSet.t -> VarSet.t -> context
end = struct
  open PM
  open Polyhedrons
  open GenericProgram_.OverApproximationUtils (A)
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


  (** Properties are a set of constraints.
      If φ is in the properties, then it's negation ¬φ should not be in the set.
      Otherwise, this φ and ¬φ would be checked twice in a refinement step. *)
  let mk_from_heuristic_scc config graph scc program_variables =
    let am = Ppl.manager_alloc_loose () in

    (* TODO eliminate non contributing variables *)
    let pr_h outgoing_transition =
      outgoing_transition |> PM.Transition.label |> PM.TransitionLabel.guard
      |> Polyhedrons.project_constraint am program_variables
      |> AtomSet.of_list
    in

    let pr_hv outgoing_transition =
      outgoing_transition |> PM.Transition.label |> PM.TransitionLabel.guard
      |> Polyhedrons.bound_variables_constraint am program_variables
      |> Polyhedrons.project_constraint am program_variables
      |> AtomSet.of_list
    in

    let pr_c incoming_transition =
      let label = PM.Transition.label incoming_transition in
      let update_approx, guard_approx = PM.TransitionLabel.update_map label |> overapprox_update in
      let guard = PM.TransitionLabel.guard label |> Guard.mk_and guard_approx in
      Polyhedrons.update_guard am update_approx guard
      |> Polyhedrons.project_constraint am program_variables
      |> AtomSet.of_list
    in

    let pr_cv incoming_transition =
      let label = PM.Transition.label incoming_transition in
      let update_approx, guard_approx = PM.TransitionLabel.update_map label |> overapprox_update in
      let guard = PM.TransitionLabel.guard label |> Guard.mk_and guard_approx in
      ApronInterface.Koat2Apron.constraint_to_polyh am guard
      |> Polyhedrons.project_polyh am program_variables
      |> Polyhedrons.bound_variables_polyh am program_variables
      |> AtomSet.of_list
    in

    let pr_d (cycle : Transition.t list) =
      (* Backward propagation captures the properties needed for a full cycle *)
      List.fold_right
        ~f:(fun transition combined ->
          Logging.log ~level:Logger.DEBUG cfr_logger "heuristic.backprop" (fun () ->
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
          |> Polyhedrons.project_constraint am program_variables)
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
      Logging.log ~level:Logger.DEBUG cfr_logger "heuristic" (fun () ->
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
        Logging.log ~level:Logger.DEBUG cfr_logger "heuristic" (fun () ->
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
                 Logging.log ~level:Logger.DEBUG cfr_logger "heuristic" (fun () ->
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
      let f = Formula.mk constr in
      (* TODO linearize constraints? *)
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
                 Logging.log ~level:Logger.DEBUG cfr_logger "abstract" (fun () ->
                     [ ("ENTAILS", Atom.to_string prop) ]);
                 Some prop)
               else if entails_prop (Atom.neg prop) then (
                 Logging.log ~level:Logger.DEBUG cfr_logger "abstract" (fun () ->
                     [ ("ENTAILS_NEG", Atom.to_string prop) ]);
                 Some (Atom.neg prop))
               else
                 None)
        |> AtomSet.of_sequence
      in
      entailed_props
    in

    match Map.find context location with
    | Some properties ->
        Logging.log ~level:Logger.DEBUG cfr_logger "abstract" (fun () ->
            [
              ("LOCATION", Location.to_string location);
              ("PROPERTIES", AtomSet.to_string ~pretty:true properties);
            ]);
        Abstr (abstract_guard_ properties guard)
    | None ->
        Logging.log ~level:Logger.DEBUG cfr_logger "abstract" (fun () ->
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
