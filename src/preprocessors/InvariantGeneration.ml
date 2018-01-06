open Batteries
open Program.Types
open Polynomials
open Atoms
open Constraints
open PolyTypes

(** Converts a koat variable to its apron equivalent. *)
let var_to_apron: Var.t -> Apron.Var.t =
  Apron.Var.of_string % Var.to_string

(** Converts a koat variable set to its apron equivalent. *)
let vars_to_apron: VarSet.t -> Apron.Var.t array =
  VarSet.map_to_array var_to_apron
  
(** Converts a koat integer to its apron equivalent. *)
(** TODO Usage of OurInt.to_int breaks usage of big_int *)
let const_to_apron: OurInt.t -> Apron.Coeff.t =
  Apron.Coeff.s_of_int % OurInt.to_int
  
let transform_program program =

  (** Creates the apron environment where all program variables are integer variables. *)
  let environment: Apron.Environment.t =
    Apron.Environment.make (vars_to_apron (Program.vars program)) [||]
  in

  (** Converts a koat polynomial to its apron equivalent. *)
  let poly_to_apron (poly: Polynomial.t): Apron.Texpr1.t =
    (* TODO Texpr1.Up: Round in which direction? *) 
    let open Apron in 
    Polynomial.fold
      ~const:(fun c ->
        Texpr1.Cst (const_to_apron c)
      )
      ~var:(fun v ->
        Texpr1.Var (var_to_apron v)
      )
      ~neg:(fun expr ->
        Texpr1.Unop (Texpr1.Neg, expr, Texpr1.Int, Texpr1.Up)
      )
      ~plus:(fun expr1 expr2 ->
        Texpr1.Binop (Texpr1.Add, expr1, expr2, Texpr1.Int, Texpr1.Up)
      )
      ~times:(fun expr1 expr2 ->
        Texpr1.Binop (Texpr1.Mul, expr1, expr2, Texpr1.Int, Texpr1.Up)
      )
      ~pow:(fun expr n ->
        Enum.repeat ~times:n expr
        |> Enum.fold
             (fun result expr -> Texpr1.Binop (Texpr1.Mul, result, expr, Texpr1.Int, Texpr1.Up))
             (Texpr1.Cst (const_to_apron OurInt.one))
      )
      poly
    |> Apron.Texpr1.of_expr environment
  in

  (** Converts a koat atom to its apron equivalent *)
  let atom_to_apron (atom: Atom.t): Apron.Tcons1.t =
    let open Apron in
    Atom.fold
      ~subject:poly_to_apron
      (* We have a<=b, Apron uses form c>=0, therefore we need 0<=b-a *)
      ~le:(fun expr1 expr2 -> Tcons1.make (Texpr1.binop Texpr1.Sub expr2 expr1 Texpr1.Int Texpr1.Up) Tcons1.SUPEQ)
      atom
  in

  (** Converts a koat constraint to its apron equivalent *)
  let constraint_to_apron (constr: Constraint.t): Apron.Tcons1.earray =
    let open Apron in
    let single_constraints =
      constr
      |> Constraint.atom_list
      |> List.map atom_to_apron
      |> Array.of_list (* Pretty dump, but more readable *)
    in
    let constraints =
      Tcons1.array_make environment (Array.length single_constraints)
    in
    (* Copy the array to an apron array, this loop seems to be the only way *)
    for i=0 to Array.length single_constraints - 1 do
      Tcons1.array_set constraints i (Array.get single_constraints i)
    done;
    constraints
  in

  (** The manager defines the abstract domain of the variables.
      The octagon domain searches for something like +-x +-y <= b *)
  let manager =
    Oct.manager_alloc ()
  in

  (** Applies the guard to the abstract value, reducing its size. *)
  let apply_guard (guard: TransitionLabel.Guard.t) (abstract: 'a Apron.Abstract1.t): 'a Apron.Abstract1.t =
    guard
    |> constraint_to_apron
    |> Apron.Abstract1.meet_tcons_array manager abstract
  in

  (** Applies the update to the abstract value. *)
  let apply_update (vars: VarSet.t) (update: Var.t -> Polynomial.t Option.t) (abstract: 'a Apron.Abstract1.t): 'a Apron.Abstract1.t =
    (* TODO Reset the temporary variables from the new design *)
    let assignments =
      vars
      |> VarSet.to_array
      |> Array.map update
      |> Array.map (Option.map poly_to_apron)
      |> Array.map Option.get (* TODO With the new design an update will be always defined *)
    in
    Apron.Abstract1.assign_texpr_array manager abstract (vars_to_apron vars) assignments None
  in

  (** Applies the transition to the abstract value.
      First, it applies the guard and then the update. *)
  let apply_transition ((l,t,l'): Transition.t) (abstract: 'a Apron.Abstract1.t): 'a Apron.Abstract1.t =
    abstract
    |> apply_guard (TransitionLabel.guard t)
    |> apply_update (Program.vars program) (TransitionLabel.update t)
  in

  let module LocationMap = Map.Make(Location) in

  (** The bottom element of the static analysis for the whole program.
      The values at the start location are defined to be not restricted in any way.
      All other values at other program locations are undefined in the beginning.
      This will change through assignments in the program. *)
  let bottom: ('a Apron.Abstract1.t) LocationMap.t =
    program
    |> Program.graph
    |> TransitionGraph.locations
    |> LocationSet.enum
    |> Enum.map (fun location ->
           if Program.is_initial_location program location then
             (location, Apron.Abstract1.top manager environment)
           else
             (location, Apron.Abstract1.bottom manager environment)
         )
    |> LocationMap.of_enum
  in

  (** Recomputes the abstract value for the single location.
      For this purpose, all incoming transitions are considered and their updated abstract value meet together in a union. *)
  let recompute_location (location: Location.t) (program_abstract :('a Apron.Abstract1.t) LocationMap.t): ('a Apron.Abstract1.t) LocationMap.t =
    location
    |> TransitionGraph.pred_e (Program.graph program)
    |> List.map (fun (l,t,l') ->
           program_abstract
           |> LocationMap.find l 
           |> apply_transition (l,t,l')
         )
    |> Array.of_list
    |> Apron.Abstract1.join_array manager
    |> (fun new_abstract -> LocationMap.modify location (Apron.Abstract1.meet manager new_abstract) program_abstract) 
  in

  (** At this point, we have a abstract bottom element for the whole program and a function, which can recompute the abstract value for a single location.
      Now it is important, to find a good sequence of locations, such that the recomputation of all locations comes as early as possible to a fixpoint.  *)
  
  (* TODO Implement *)
  MaybeChanged.same program
