(** Implemenation of a preprocessor which adds invariants to transitions.*)
open OurBase
open Polynomials
open Constraints

(** This preprocessor generates invariants for a given program and adds them to the transitions of the program.
    This way more restrictive information is available locally in the transitions. *)

(** Logger Preprocessor *)
let logger = Logging.(get Preprocessor)

module Make(M: ProgramTypes.ClassicalProgramModules) = struct
  open M

  (** A map from program locations to anything. *)
  type 'a location_map = (Location.t,'a) Hashtbl.t
  type 'a transition_map = (Transition.t,'a) Hashtbl.t

  (* The number of steps per transition to be performed during fixpoint iteration without widening*)
  let max_steps_without_widening = 5

  (** An abstract value for all values of variable at every program location. *)
  type 'a program_abstract = ('a Apron.Abstract1.t) location_map

  let transform_program_ program_ (program: Program.t) =
    let open ApronInterface.Koat2Apron in
    let open ApronInterface.Apron2Koat in

    let transitions = Program.transitions program_ in
    let locations = Program.locations program_ in

    let vars = List.fold ~f:(fun vars (_,t,_) -> Set.union vars (TransitionLabel.vars_without_memoization t)) ~init:VarSet.empty (Set.to_list transitions) in

    (** Creates the apron environment where all program_ variables are integer variables. *)
    let environment: Apron.Environment.t =
      Apron.Environment.make (vars_to_apron vars) [||]
    in

    (** The manager defines the abstract domain of the variables.
        The octagon domain searches for something like +-x +-y <= b *)
    let manager =
      Oct.manager_alloc ()
    in

    (** Applies the guard to the abstract value, reducing its size. *)
    let apply_guard (guard: Guard.t) (abstract: 'a Apron.Abstract1.t): 'a Apron.Abstract1.t =
      constraint_to_apron environment guard
      |> Apron.Abstract1.meet_tcons_array manager abstract
    in

    (** Applies the update to the abstract value. *)
    let apply_update (update: Var.t -> Polynomial.t Option.t) (abstract: 'a Apron.Abstract1.t): 'a Apron.Abstract1.t =
      (* A completely unbound (non-deterministic) choice *)
      let any_value = Apron.(Texpr1.cst environment (Coeff.Interval Interval.top)) in
      let assignments =
        vars
        |> Set.to_array
        |> Array.map ~f:update
        |> Array.map ~f:(Option.value_map ~f:(poly_to_apron environment) ~default:any_value)
      in
      Apron.Abstract1.assign_texpr_array manager abstract (vars_to_apron vars) assignments None
      (* This is somehow necessary otherwise apron gets confused over variables with the same name (i.e. the same variables).  *)
      |> Apron.Abstract1.to_tcons_array manager
      |> constraint_from_apron
      |> constraint_to_apron environment
      |> Apron.Abstract1.of_tcons_array manager environment
    in

    (** Applies the transition to the abstract value.
        First, it applies the guard and then the update. *)
    let apply_transition ((l,t,l'): Transition.t) (abstract: 'a Apron.Abstract1.t): 'a Apron.Abstract1.t =
      abstract
      |> apply_guard (TransitionLabel.guard t)
      |> apply_update (TransitionLabel.update t)
    in

    (** The bottom element of the static analysis for the whole program.
        The values at the start location are defined to be not restricted in any way.
        All other values at other program locations are undefined in the beginning.
        This will change through assignments in the program. *)
    let bottom(*: 'a program_abstract*) =
      Set.to_list locations
      |> List.map ~f:(fun location ->
            if Program.is_initial_location program_ location then
                (location, Apron.Abstract1.top manager environment)
            else
              (location, Apron.Abstract1.bottom manager environment)
          )
      |> Hashtbl.of_alist_exn (module Location)
    in

    let find_fixpoint (program_abstract: 'a program_abstract): 'a program_abstract =
      (* TODO At this point, we have an abstract bottom element for the whole program and a function, which can recompute the abstract value for a single location.
        Now it is important, to find a good sequence of locations, such that the recomputation of all locations comes as early as possible to a fixpoint.  *)
      (* TODO Maybe it is better to recompute transitions instead of locations. *)
      (** We use a modifiable stack here for performance reasons. *)
      let worklist: Transition.t Stack.t =
        Set.to_list transitions
        |> Stack.of_list
      in
      let transition_steps =
        Hashtbl.create ~size:(Set.length transitions) (module Transition)
        |> tap (fun m -> Set.iter ~f:(fun t -> Hashtbl.add_exn m ~key:t ~data:0) transitions)
      in

      let narrowing_abstract =
        let location_abstract l =
          Set.filter ~f:(fun (_,_,l') -> Location.equal l l') transitions
          |> Set.to_sequence
          |> Sequence.map ~f:(fun t -> apply_transition t (Apron.Abstract1.top manager environment))
          |> Sequence.fold ~f:(Apron.Abstract1.join manager) ~init:(Apron.Abstract1.bottom manager environment)
        in
        Set.to_list locations
        (* The initial location does not have any ingoing transitions *)
        |> List.filter ~f:(not % Program.is_initial_location program_)
        |> List.map ~f:(fun location -> (location, location_abstract location))
        |> Hashtbl.of_alist_exn (module Location)
      in

      while not (Stack.is_empty worklist) do
        let (l,t,l') = Stack.pop_exn worklist in
        let transfered_l_abstract = apply_transition (l,t,l') (Hashtbl.find_exn program_abstract l) in
        let old_abstract = Hashtbl.find_exn program_abstract l' in

        if not (Apron.Abstract1.is_leq manager transfered_l_abstract old_abstract) then (
          Hashtbl.update program_abstract l' ~f:(fun _ ->
            if Hashtbl.find_exn transition_steps (l,t,l') < max_steps_without_widening then (
              Hashtbl.update transition_steps (l,t,l') ~f:((+) 1 % Option.value_exn);
              Apron.Abstract1.join manager old_abstract transfered_l_abstract
            ) else
              Apron.Abstract1.meet manager
                (Apron.Abstract1.widening manager old_abstract transfered_l_abstract)
                (Hashtbl.find_exn narrowing_abstract l')
          );

          (* add succesor transitons to worklist *)
          TransitionGraph.succ_e (Program.graph program_) l'
          |> List.iter ~f:(fun transition ->
                (*TODO comparison faster with 2 component transition id*)
                if not (List.exists ~f:(Transition.equal transition) (Stack.to_list worklist)) then
                  Stack.push worklist transition
              )
        );
      done;
      program_abstract
    in

    (** Converts a value of the abstract domain into a koat constraint which acts as an invariant. *)
    let extract_invariant (abstract: 'a Apron.Abstract1.t): Constraint.t =
      abstract
      |> Apron.Abstract1.to_tcons_array manager
      |> constraint_from_apron
    in

    (** Determines for each location its invariant constraint and filters locations, which does not have an additional invariant. *)
    let invariants =
      bottom
      |> find_fixpoint
      |> Hashtbl.map ~f:extract_invariant
      |> Hashtbl.filter ~f:(not % Constraint.is_true)
    in

    if Hashtbl.is_empty invariants then
      MaybeChanged.same program
    else
      let add location invariant program =
        Logger.(log logger INFO (fun () -> "add_invariant", ["location", Location.to_string location; "invariant", Constraint.to_string invariant]));
        ProofOutput.add_str_paragraph_to_proof (fun () -> "Found invariant " ^ Constraint.to_string ~pretty:true invariant ^ " for location "^Location.to_string location);
        Program.add_invariant location invariant program
      in
      Hashtbl.fold ~f:(fun ~key ~data -> add key data) invariants ~init:program
      |> MaybeChanged.changed (* TODO Actually, we should check if the new invariant is already implied and only then say, that it is changed. *)

  let transform_program program =
    transform_program_ (Program.map_labels TransitionLabel.overapprox_nonlinear_updates program) program

end

include Make(ProgramModules)
