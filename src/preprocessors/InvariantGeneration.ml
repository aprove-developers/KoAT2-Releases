(** Implemenation of a preprocessor which adds invariants to transitions.*)
open Batteries
open ProgramModules
open Polynomials
open Constraints

(** This preprocessor generates invariants for a given program and adds them to the transitions of the program.
    This way more restrictive information is available locally in the transitions. *)

(** Logger Preprocessor *)
let logger = Logging.(get Preprocessor)

(** A map from program locations to anything. *)
module LocationMap = Hashtbl.Make(Location)
module TransitionMap = Hashtbl.Make(Transition)

(* The number of steps per transition to be performed during fixpoint iteration without widening*)
let max_steps_without_widening = 5

(** An abstract value for all values of variable at every program location. *)
type 'a program_abstract = ('a Apron.Abstract1.t) LocationMap.t

let transform_program_ program_ program =

  let open ApronInterface.Koat2Apron in
  let open ApronInterface.Apron2Koat in

  let locations =
    Program.graph program_
    |> TransitionGraph.locations
  in

  let transitions = Program.transitions program_ in
  let vars = List.fold (fun vars (_,t,_) -> VarSet.union vars (TransitionLabel.vars_without_memoization t)) VarSet.empty (transitions |> TransitionSet.to_list) in

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
  let apply_guard (guard: TransitionLabel.Guard.t) (abstract: 'a Apron.Abstract1.t): 'a Apron.Abstract1.t =
    guard
    |> constraint_to_apron environment
    |> Apron.Abstract1.meet_tcons_array manager abstract
  in

  (** Applies the update to the abstract value. *)
  let apply_update (update: Var.t -> Polynomial.t Option.t) (abstract: 'a Apron.Abstract1.t): 'a Apron.Abstract1.t =
    (* A completely unbound (non-deterministic) choice *)
    let any_value = Apron.(Texpr1.cst environment (Coeff.Interval Interval.top)) in
    let assignments =
      vars
      |> VarSet.to_array
      |> Array.map update
      |> Array.map (Option.map_default (poly_to_apron environment) any_value)
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
    |> tap ( fun a' ->
        let to_string a = Apron.Abstract1.print Format.str_formatter a; Format.flush_str_formatter () in
        Printf.printf "apply_transition %s to %s obtained %s\n" (Transition.to_string_pretty (l,t,l')) (to_string abstract) (to_string a')
       )
  in

  (** The bottom element of the static analysis for the whole program.
      The values at the start location are defined to be not restricted in any way.
      All other values at other program locations are undefined in the beginning.
      This will change through assignments in the program. *)
  let bottom: 'a program_abstract =
    locations
    |> LocationSet.enum
    |> Enum.map (fun location ->
           if Program.is_initial_location program_ location then
              (location, Apron.Abstract1.top manager environment)
           else
             (location, Apron.Abstract1.bottom manager environment)
         )
    |> LocationMap.of_enum
  in

  let find_fixpoint (program_abstract: 'a program_abstract): 'a program_abstract =
    (* TODO At this point, we have an abstract bottom element for the whole program and a function, which can recompute the abstract value for a single location.
       Now it is important, to find a good sequence of locations, such that the recomputation of all locations comes as early as possible to a fixpoint.  *)
    (* TODO Maybe it is better to recompute transitions instead of locations. *)
    (** We use a modifiable stack here for performance reasons. *)
    let worklist: Transition.t Stack.t =
      program_
      |> Program.transitions
      |> TransitionSet.enum
      |> Stack.of_enum
    in
    let transition_steps =
      TransitionMap.create (TransitionSet.cardinal transitions)
      |> tap (fun m -> TransitionSet.iter (fun t -> TransitionMap.add m t 0) transitions)
    in

    let narrowing_abstract =
      let location_abstract l =
        TransitionSet.filter (fun (_,_,l') -> Location.equal l l') transitions
        |> TransitionSet.enum
        |> Enum.map (fun t -> apply_transition t (Apron.Abstract1.top manager environment))
        |> Enum.fold (Apron.Abstract1.join manager) (Apron.Abstract1.bottom manager environment)
      in
      LocationSet.enum locations
      (* The initial location does not have any ingoing transitions *)
      |> Enum.filter (not % Program.is_initial_location program_)
      |> Enum.map (fun location -> (location, location_abstract location))
      |> LocationMap.of_enum
    in

    while not (Stack.is_empty worklist) do
      let (l,t,l') = Stack.pop worklist in
      let transfered_l_abstract = apply_transition (l,t,l') (LocationMap.find program_abstract l) in
      let old_abstract = LocationMap.find program_abstract l' in

      if not (Apron.Abstract1.is_leq manager transfered_l_abstract old_abstract) then (
        LocationMap.modify l' (fun _ ->
          if TransitionMap.find transition_steps (l,t,l') < max_steps_without_widening then (
            TransitionMap.modify (l,t,l') (Int.add 1) transition_steps;
            Apron.Abstract1.join manager old_abstract transfered_l_abstract
          ) else
            Apron.Abstract1.meet manager
              (Apron.Abstract1.widening manager old_abstract transfered_l_abstract)
              (LocationMap.find narrowing_abstract l')
        ) program_abstract;

        (* add succesor transitons to worklist *)
        TransitionGraph.succ_e (Program.graph program_) l'
        |> List.iter (fun transition ->
              (*TODO comparison faster with 2 component transition id*)
               if not (Enum.exists (Transition.equivalent transition) (Stack.enum worklist)) then
                 Stack.push transition worklist
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
    |> LocationMap.map (fun _ -> extract_invariant)
    |> LocationMap.filter (not % Constraint.is_true)
  in

  if LocationMap.is_empty invariants then
    MaybeChanged.same program
  else
    let add location invariant program =
      Logger.(log logger INFO (fun () -> "add_invariant", ["location", Location.to_string location; "invariant", Constraint.to_string invariant]));
      ProofOutput.add_str_paragraph_to_proof (fun () -> "Found invariant " ^ Constraint.to_string ~pretty:true invariant ^ " for location "^Location.to_string location);
      Program.add_invariant location invariant program
    in
    program
    |> LocationMap.fold add invariants
    |> MaybeChanged.changed (* TODO Actually, we should check if the new invariant is already implied and only then say, that it is changed. *)

let transform_program program =
  Printf.printf "Here\n";
  let transitions = TransitionSet.map (Tuple3.map identity TransitionLabel.overapprox_nonlinear_updates identity) (Program.transitions program) in
  let program_ = Program.from (List.map List.singleton (transitions |> TransitionSet.to_list)) (Program.start program) in
  transform_program_ program_ program
