open Batteries

module Loops (PM : ProgramTypes.ProgramModules) = struct
  open PM

  (* mutable state *)
  type state = {
    blocked : (Location.t, bool) Hashtbl.t;
    b_lists : (Location.t, Location.t list) Hashtbl.t;
  }

  (** Finds all loops in a graph, using the algorithm from Donald B. Johnson (1975)
  By itself this function is probably not very useful. Use transition_loops_for in 
  order to get the loops with transitions.
  *)
  let find_loops graph =
    let all_locations = TransitionGraph.locations graph in

    (* initialize hashtables to the number of locations in the graph *)
    let hashtbl_size = all_locations |> LocationSet.enum |> Enum.hard_count in

    (* Initial state for circuit, based on a set of locations *)
    let initial_state locations =
      let state =
        {
          blocked = Hashtbl.create hashtbl_size;
          b_lists = Hashtbl.create hashtbl_size;
        }
      in
      LocationSet.iter
        (fun location ->
          Hashtbl.add state.blocked location true;
          Hashtbl.add state.b_lists location [])
        locations;
      state
    in

    (* Check if a location is blocked, doesn't mutate s *)
    let is_blocked s location =
      Option.default false (Hashtbl.find_option s.blocked location)
    in

    (* Get the b_list of a location, doesn't mutate s *)
    let b_list_of s location =
      Option.default [] (Hashtbl.find_option s.b_lists location)
    in

    (* Add a location v to the b_list of w, if not already present; mutates s *)
    let add_to_b_list s w v =
      let old_b_list = b_list_of s w in
      if not (List.mem v old_b_list) then
        Hashtbl.replace s.b_lists w (v :: old_b_list)
    in

    (* Block a location, mutates s *)
    let block s location = Hashtbl.replace s.blocked location true in

    (* Unblock a location, and clear b_lists, mutates s *)
    let rec unblock s location =
      if is_blocked s location then Hashtbl.replace s.blocked location false;
      List.iter (unblock s)
        (Option.default [] (Hashtbl.find_option s.b_lists location));
      Hashtbl.replace s.b_lists location []
    in

    (* Transform the path into a list of locations, without mutating the path *)
    let loop_of path = Stack.enum path |> Enum.fold (Fun.flip List.cons) [] in

    (* We start with an empty path *)
    let path = Stack.create () in

    let rec circuit graph s prev_results start_location current_location =
      Stack.push current_location path;
      block s current_location;
      let closed, new_results =
        TransitionGraph.fold_succ
          (fun next_location (closed_acc, loops_acc) ->
            if Location.equal next_location start_location then
              (* Found a loop, add to results *)
              (true, loop_of path :: loops_acc)
            else if not (is_blocked s next_location) then
              let inner_closed, inner_loops_acc =
                circuit graph s loops_acc start_location next_location
              in
              (closed_acc || inner_closed, inner_loops_acc)
            else (closed_acc, loops_acc))
          graph current_location (false, prev_results)
      in

      if closed then unblock s current_location
      else
        TransitionGraph.iter_succ
          (fun w -> add_to_b_list s w current_location)
          graph current_location;

      let _ = Stack.pop path in
      (closed, new_results)
    in

    (* Locations are ordered; as is the LocationSet *)
    let _, results =
      LocationSet.fold
        (fun location (current_graph, prev_results) ->
          (* The SCC containing the smallest location according to the ordering *)
          let min_scc_opt =
            List.find_opt
              (fun scc ->
                TransitionSet.locations scc |> LocationSet.mem location)
              (TransitionGraph.sccs current_graph)
          in
          (* the smallest location might be in a trivial scc and already filtered by TransitionGraph.sccs *)
          let new_results =
            match min_scc_opt with
            | Some scc ->
                let scc_graph = TransitionGraph.mk (TransitionSet.enum scc)
                and scc_locations = TransitionSet.locations scc in
                let _closed, results =
                  circuit scc_graph
                    (initial_state scc_locations)
                    prev_results location location
                in
                results
            | None -> prev_results
          in
          (TransitionGraph.remove_vertex current_graph location, new_results))
        (TransitionGraph.locations graph)
        (graph, [])
    in
    results

  (** Find all loops in a given scc using the algorithm from Donald B. Johnson (1975) **)
  let find_loops_scc graph scc =
    let scc_graph = TransitionSet.enum scc |> TransitionGraph.mk in
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
          if Location.equal (Transition.target t) target then t :: ts else ts)
        graph src []
    in

    let combine (transitions : Transition.t list)
        (suffixes : Transition.t list list) : Transition.t list list =
      List.fold
        (fun results suffix ->
          List.fold
            (fun results transition -> (transition :: suffix) :: results)
            [] transitions)
        [] suffixes
    in

    (* computes for every step in the loop the walkable transitions *)
    let rec transition_loops (loc_loop : Location.t list) =
      match loc_loop with
      | l1 :: l2 :: ls ->
          combine
            (transitions_betwen_locations l1 l2)
            (transition_loops (l2 :: ls))
      | l1 :: [] -> [ [] ]
      | [] -> [ [] ]
    in

    List.fold
      (fun results loc_loop -> List.append (transition_loops loc_loop) results)
      [] loc_loops
end

module FVS (PM : ProgramTypes.ProgramModules) = struct
  module Loops = Loops (PM)
  (* TODO: move this module to SMT module *)

  open Z3
  open PM

  exception FVSFailed

  (** Compute the Feedback-vertex Set for a given graph. If loops have already
      been computed you can give them to this function to avoid recomputation.
      *)
  let fvs graph ?(loops = None) =
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
      match loops with Some lps -> lps | None -> Loops.find_loops graph
    in

    let locations = TransitionGraph.locations graph in

    (* initialize hashtables to the number of locations in the graph *)
    let location_var_map =
      locations |> LocationSet.enum |> Enum.hard_count |> Hashtbl.create
    in

    (* create and remember a (z3) integer variable for every location. it will
       represent if the location is part of the FVS or Hitting Set *)
    LocationSet.iter
      (fun location ->
        location |> Location.to_string
        |> Arithmetic.Integer.mk_const_s ctx
        |> Hashtbl.add location_var_map location)
      locations;

    (* get the integer variable representing a location *)
    let var_for location = Hashtbl.find location_var_map location in

    let zero = Arithmetic.Integer.mk_numeral_i ctx 0
    and one = Arithmetic.Integer.mk_numeral_i ctx 1 in

    (* create an optimization problem *)
    let o = Optimize.mk_opt ctx in

    (* restrict every variable to be in {0, 1} *)
    Hashtbl.values location_var_map
    |> Enum.fold
         (fun constraints var ->
           let ge0 = Z3.Arithmetic.mk_ge ctx var zero
           and le1 = Z3.Arithmetic.mk_le ctx var one in
           ge0 :: le1 :: constraints)
         []
    |> Optimize.add o;

    (* require every loop to contain one marked/hit location *)
    List.fold
      (fun constraints loop ->
        let loop_constraint =
          List.fold
            (fun expressions location -> var_for location :: expressions)
            [] loop
          |> Z3.Arithmetic.mk_add ctx
          |> Z3.Arithmetic.mk_le ctx one
        in
        loop_constraint :: constraints)
      [] loops
    |> Optimize.add o;

    String.println stdout (Optimize.to_string o);

    (* Solve ILP, minimizing the number of marked locations *)
    let _handle =
      Hashtbl.values location_var_map
      |> List.of_enum |> Z3.Arithmetic.mk_add ctx |> Optimize.minimize o
    in

    let model =
      match Optimize.get_model o with Some m -> m | None -> raise FVSFailed
    in

    String.println stdout (Model.to_string model);

    let fvs_solution =
      Hashtbl.fold
        (fun location var fvs_solution ->
          match Model.get_const_interp_e model var with
          | Some value ->
              if Expr.equal value one then LocationSet.add location fvs_solution
              else fvs_solution
          | None -> raise FVSFailed)
        location_var_map LocationSet.empty
    in

    String.println stdout (LocationSet.to_string fvs_solution);

    fvs_solution
end

(* TODO: Move to ProgramModules and/or Polynomials *)

(** An generic way of overapproximating polynomials over indeterminates is required *)
module type OverApproximation = sig
  type t
  type approx = Polynomials.Polynomial.t * Guard.t

  val overapprox_indeterminates : t -> approx
end

(* TODO: Move to ProgramModules and/or Polynomials *)

(** Extension of the ProgramModules with generic overapproximation *)
module type ProgramModulesWithApprox = sig
  include ProgramTypes.ProgramModules
  module OverApproximation : OverApproximation with type t := UpdateElement.t
end

(* TODO: Move to ProgramModules and/or Polynomials *)

(** Trivial implementation of overapproxmation in classical programs *)
module ClassicProgramModulesWithApprox : ProgramModulesWithApprox = struct
  include ProgramModules

  module OverApproximation = struct
    type t = UpdateElement.t
    type approx = Polynomials.Polynomial.t * Guard.t

    (** Overapproximating of normal polynomials is not required and the polynomial is returned as is *)
    let overapprox_indeterminates poly = (poly, Guard.mk_true)
  end
end

(* TODO: Move to ProgramModules and/or Polynomials *)

(** Use already existing overapproximation *)
module ProbabilisticProgramModulesWithApprox : ProgramModulesWithApprox = struct
  include ProbabilisticProgramModules

  module OverApproximation = struct
    type t = UpdateElement.t
    type approx = Polynomials.Polynomial.t * Guard.t

    module P = Polynomials.Polynomial

    let overapprox_indeterminates =
      UpdateElement.fold
        ~const:(fun c -> (P.of_constant c, Guard.mk_true))
        ~indeterminate:(fun i ->
          match i with
          | UpdateElement_.UpdateValue.Var v -> (P.of_var v, Guard.mk_true)
          | UpdateElement_.UpdateValue.Dist d ->
              let new_var = Var.fresh_id Var.Int () in
              let guard = ProbabilityDistribution.as_guard d new_var in
              (P.of_var new_var, guard))
        ~neg:(fun (p, g) -> (P.neg p, g))
        ~plus:(fun (lp, lg) (rp, rg) -> (P.(lp + rp), Guard.mk_and lg rg))
        ~times:(fun (lp, lg) (rp, rg) -> (P.(lp * rp), Guard.mk_and lg rg))
        ~pow:(fun (p, g) exp -> (P.pow p exp, g))
  end
end

module Unfolding (PM : ProgramModulesWithApprox) = struct
  open PM
  open Apron
  module VarMap = ProgramTypes.VarMap

  type polyhedron
  type guard = Constraints.Constraint.t
  type update = UpdateElement.t VarMap.t

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
    let prev_env = Abstract1.env polyh in
    let apron_vars = ApronInterface.Koat2Apron.vars_to_apron vars in
    (* Apron would panic if the variable is alread in the environment *)
    let new_apron_vars =
      Array.filter (not % Environment.mem_var prev_env) apron_vars
    in
    let new_env = Environment.add prev_env new_apron_vars [||] in

    (* since we only add new variables projecting on the 0-plane or not doesn't matter *)
    Abstract1.change_environment am polyh new_env true

  (** project a polyhedron onto the given variable dimensions 
  Does not add new dimensions for variables not in the polyhedron
   *)
  let project_polyh am vars polyh =
    let prev_env = Abstract1.env polyh in
    let apron_vars =
      ApronInterface.Koat2Apron.vars_to_apron vars
      |> Array.filter (not % Environment.mem_var prev_env)
    in
    let new_env = Environment.make apron_vars [||] in
    (* Implicitly projects all removed dimensions *)
    Abstract1.change_environment am polyh new_env false

  (** Intersect a polynomial with a constraint *)
  let meet am constr polyh =
    let env = Abstract1.env polyh in
    let apron_expr = ApronInterface.Koat2Apron.constraint_to_apron env constr in
    Abstract1.meet_tcons_array am polyh apron_expr

  (* TODO: move to K2A *)

  (** converts an update map over polynomials to apron arrays *)
  let update_to_apron env
      (update : Polynomials.Polynomial.t ProgramTypes.VarMap.t) =
    VarMap.enum update
    |> Enum.map (fun (var, ue) ->
           let apron_var = ApronInterface.Koat2Apron.var_to_apron var
           and apron_expr = ApronInterface.Koat2Apron.poly_to_apron env ue in
           (apron_var, apron_expr))
    |> Array.of_enum |> Array.split

  let vars_in_update update =
    VarMap.fold
      (fun var ue vars ->
        vars |> VarSet.add var |> VarSet.union (Polynomials.Polynomial.vars ue))
      update VarSet.empty

  let update_polyh am update polyh =
    let env = Abstract1.env polyh in
    let vars_arr, texpr_arr = update_to_apron env update in
    Abstract1.assign_texpr_array am polyh vars_arr texpr_arr None

  let unfold am polyh program_vars guard invariant update =
    let update_approx, update_guard =
      VarMap.fold
        (fun var ue (new_update, guards) ->
          let ue_approx, guard =
            OverApproximation.overapprox_indeterminates ue
          in
          (VarMap.add var ue_approx new_update, Guard.mk_and guards guard))
        update
        (VarMap.empty, Guard.mk_true)
    in
    let temp_vars =
      [
        Guard.vars guard;
        Guard.vars invariant;
        Guard.vars update_guard;
        vars_in_update update_approx;
      ]
      |> List.fold VarSet.union VarSet.empty
      |> VarSet.filter (fun v -> VarSet.mem v program_vars)
    in

    polyh
    |> add_vars_to_polyh am temp_vars
    |> meet am (Guard.all [ guard; invariant; update_guard ])
    |> update_polyh am update_approx
    |> project_polyh am program_vars
end

(** Generic type for abstractions *)
module type Abstraction = sig

  (** Context of the abstraction, for example properties *)
  type context 

  (** Constraint type *)
  type guard = Constraints.Constraint.t

  type location = Location.t

  (** The type to which the constraint is abstracted to *)
  type abstracted [@@derive eq, ord]

  type t =
    | Abstr of abstracted
    | Constr of guard
    [@@deriving eq, ord]

  (** Abstract a constraint to the abstracted type. Can return None, when the
 constraint is not satisfiable *)
  val abstract: context -> location -> guard -> t option

  (* val to_string: t -> string *)

  val to_guard: t -> guard
end

module PropertyBasedAbstraction : Abstraction = struct 
  open Formulas
  open Constraints
  open Atoms

  module AtomSet = Set.Make (Atom)
  module LocationMap = Map.Make(Location)

  (** Contains a set of properties for every location that should be
      abstracted, every missing location shall not be abstracted *)
  type context = AtomSet.t LocationMap.t

  type guard = Constraint.t
    [@@deriving eq, ord]
  type location = Location.t

  (** The abstracted type guard type *)
  type abstracted = AtomSet.t[@@deriving eq, ord]

  (** Marks if the guard was abstracted or not *)
  type t =
    | Abstr of abstracted
    | Constr of guard
    [@@deriving eq, ord]

  (** Properties are a set of constraints. If φ is in the properties, then it's
      negation ¬φ should not, because it is already checked as well. Adding it
      though would just result in double checks (overhead) *)
  let mk_properties props_list =
    List.fold
      (fun props prop ->
        (* Add property only, if its negation is not already present *)
        if not (AtomSet.mem (Atom.neg prop) props) then AtomSet.add prop props
        else props)
      AtomSet.empty props_list


  let abstract context location guard = 
    (** Identifies all properties that are entailed by the constraint, implicit SAT check *)
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
          (* Printf.printf "%s && %s is %s\n" (Formula.to_string constr_formula) (Formula.to_string neg_atom) (if is_unsat then "UNSAT" else "SAT"); *)
          pop solver;
          is_unsat
        in

        (* No need to check entailment, when constraint is already UNSAT *)
        if unsatisfiable solver then None
          (* Check entailment for every propery it's negation *)
        else
          let entailed_props =
            AtomSet.enum properties
            |> Enum.filter_map (fun prop ->
                   if entails_prop prop then
                     (* Printf.printf "%s entails %s\n" *)
                     (* (Constraint.to_string constr) *)
                     (* (Atom.to_string prop); *)
                     Some prop
                   else if entails_prop (Atom.neg prop) then
                     (* Printf.printf "%s entails %s\n" *)
                     (* (Constraint.to_string constr) *)
                     (* (Atom.to_string (Atom.neg prop)); *)
                     Some (Atom.neg prop)
                   else None)
            |> AtomSet.of_enum
          in
          Some entailed_props)
    in 

    match LocationMap.find_opt location context with 
    | Some properties -> abstract_guard_ properties guard |> Option.map (fun a -> Abstr a)
    | None -> if Formula.mk guard |> SMT.Z3Solver.satisfiable then Some (Constr guard) else None

  let to_guard a = match a with 
  | Abstr abstracted -> AtomSet.to_list abstracted |> Constraint.mk
  | Constr guard -> guard
end

(** A version is a location with an (possibly abstracted) constraint used
  in the partial evaluation graph *)
module Version(A: Abstraction) = struct
  type location = Location.t
  type t = Location.t * A.t[@@ deriving eq, ord]
end

