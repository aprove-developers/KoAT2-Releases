open Batteries
open Constraints
open Formulas
open Util

exception RecursionNotSupported

module Make(TL: ProgramTypes.TransitionLabel)
           (T: ProgramTypes.Transition with type transition_label = TL.t)
           (L: ProgramTypes.Location with type t = T.location)
           (G: ProgramTypes.TransitionGraph with type location = L.t
                                             and type location_set = Set.Make(L).t
                                             and type transition_label = TL.t
                                             and type transition_set = Transition_.TransitionSetOver(T)(L).t) =
struct
  type location = L.t
  type transition_label = TL.t
  type transition = L.t * TL.t * L.t
  type location_set = Set.Make(L).t
  type transition_set = Transition_.TransitionSetOver(T)(L).t
  type transition_graph = G.t

  module LocationSet = Set.Make(L)
  module TransitionSet = Transition_.TransitionSetOver(T)(L)

  type t = {
      graph: G.t;
      start: L.t;
    }

  let start program = program.start

  let graph g = g.graph

  let equal equal_graph program1 program2 =
    equal_graph program1.graph program2.graph
    && L.equal program1.start program2.start

  let equivalent =
    equal G.equivalent

  let remove_location program location =
    { program with graph = G.remove_vertex program.graph location }

  let remove_transition program transition =
    { program with graph = G.remove_edge_e program.graph transition }

  (* Removes the transitions from a certain transitionset to a program *)
  let remove_transition_set (transitions:  TransitionSet.t) (program: t)  =
    TransitionSet.fold (flip remove_transition) transitions program

  let map_graph f program =
    { program with graph = f program.graph }

  let map_transitions f = map_graph (G.map_transitions f)

  let map_labels f = map_transitions (fun(l,t,l') -> l,f t,l')

  let locations = G.locations % graph

  let transitions =
    G.transitions % graph

  let simplify_all_guards: t -> t =
    map_transitions (T.map_label (TL.map_guard Guard.simplify))

  let vars program =
    transitions program
    |> TransitionSet.enum
    |> Enum.map T.label
    |> Enum.map TL.vars
    |> Enum.fold VarSet.union VarSet.empty

  let input_vars program =
    transitions program
    |> TransitionSet.enum
    |> Enum.map T.label
    |> Enum.map TL.input_vars
    |> Enum.fold VarSet.union VarSet.empty

  let temp_vars =
    fun program -> VarSet.diff (vars program) (input_vars program)

  let from_graph start graph =
    if List.is_empty (G.pred_e graph start) then
      { start; graph; }
    else raise (Failure "Transition leading back to the initial location.")

  let from_enum start =
    from_graph start % G.mk

  let cardinal_vars program =
    VarSet.cardinal (vars program)

  let pre program (l,t,_) =
    let is_satisfiable f =
      try SMT.Z3Solver.satisfiable f
      with SMT.SMTFailure _ -> true (* thrown if solver does not know a solution due to e.g. non-linear arithmetic *)
    in
    l
    |> G.pred_e (graph program)
    |> List.enum
    |> Enum.filter (fun (_,t',_) ->
           TL.chain_guards t' t
           |> is_satisfiable % Formula.mk % Constraint.drop_nonlinear (* such that Z3 uses QF_LIA*)
         )

  let pre_cache: (int, TransitionSet.t) Hashtbl.t = Hashtbl.create 10
  let pre_transitionset_cached program = Util.memoize pre_cache ~extractor:T.id (TransitionSet.of_enum % pre program)
  let reset_pre_cache () = Hashtbl.clear pre_cache

  let succ program (_,t,l') =
    G.succ_e (graph program) l'
    |> List.enum
    |> Enum.filter (fun (_,t',_) ->
           TL.chain_guards t t'
           |> Formula.mk
           |> SMT.Z3Solver.satisfiable
         )

  let sccs program =
    let module SCC = Graph.Components.Make(G) in
    SCC.scc_list program.graph
    |> List.rev
    |> List.enum
    |> Enum.map (G.loc_transitions program.graph)
    |> Enum.filter (not % TransitionSet.is_empty)

  let cardinal_trans_scc program =
    Enum.fold (fun counter scc -> let cardinal = (TransitionSet.cardinal scc) in counter + if cardinal > 1 then cardinal else 0) 0 (sccs program)

  let parallel_transitions graph (l,_,l') =
    transitions graph
      |> TransitionSet.filter (fun (l1,_,l1') ->  L.equal l l1 && L.equal l' l1')

  let non_trivial_transitions =
    Enum.fold TransitionSet.union TransitionSet.empty % sccs

  let add_invariant location invariant =
    map_graph (G.add_invariant location invariant)

  let is_initial program trans =
    L.(equal (program.start) (T.src trans))

  let is_initial_location program location =
    L.(equal (program.start) location)

  let to_formatted_string ?(pretty=false) program =
    let transitions =
      G.fold_edges_e (fun t str -> str @ [if pretty then T.to_string_pretty t else T.to_string t]) program.graph []
      |> FormattedString.mappend % List.map FormattedString.mk_str_line
    in
    let locations = String.concat ", " (G.fold_vertex (fun l str -> str @ [(L.to_string l)]) program.graph []) in
    FormattedString.format_append (
      [
        "Start:  "^L.to_string program.start;
        "Program_Vars:  "^(input_vars program |> VarSet.map_to_list (Var.to_string ~pretty) |> String.concat ", ");
        "Temp_Vars:  "^(temp_vars program |> VarSet.map_to_list (Var.to_string ~pretty) |> String.concat ", ");
        "Locations:  "^locations;
        "Transitions:";
      ] |> List.map (FormattedString.mk_str_line) |> FormattedString.mappend)
    transitions

  let to_string = FormattedString.render_string % to_formatted_string

  let to_simple_string program =
    G.fold_edges_e (fun t str -> str ^ ", " ^ T.to_string t) program.graph ""

  (** All entry transitions of the given transitions.
      These are such transitions, that can occur immediately before one of the transitions, but are not themselves part of the given transitions. *)
  let entry_transitions logger (program: t) (rank_transitions: T.t list): T.t List.t =
    rank_transitions
    |> List.enum
    |> Enum.map (pre program)
    |> Enum.flatten
    |> Enum.filter (fun r ->
           rank_transitions
           |> List.enum
           |> Enum.for_all (not % T.same r)
         )
    |> Enum.uniq_by T.same
    |> List.of_enum
    |> tap (fun transitions -> Logger.log logger Logger.DEBUG
                                 (fun () -> "entry_transitions", ["result", transitions |> List.enum |> Util.enum_to_string T.to_id_string]))

  (** All outgoing transitions of the given transitions.
      These are such transitions, that can occur immediately after one of the transitions, but are not themselves part of the given transitions. *)
  let outgoing_transitions logger (program: t) (rank_transitions: T.t list): T.t List.t =
    rank_transitions
    |> List.enum
    |> Enum.map (succ program)
    |> Enum.flatten
    |> Enum.filter (fun r ->
           rank_transitions
           |> List.enum
           |> Enum.for_all (not % T.same r)
         )
    |> Enum.uniq_by T.same
    |> List.of_enum
    |> tap (fun transitions -> Logger.log logger Logger.DEBUG
                                 (fun () -> "outgoing_transitions", ["result", transitions |> List.enum |> Util.enum_to_string T.to_id_string]))
end

module ProgramOverLocation(L: ProgramTypes.Location) =
  Make(TransitionLabel_) (Transition_.TransitionOver(TransitionLabel_)(L)) (L)
      (TransitionGraph_.TransitionGraphOverLocation(L))

include ProgramOverLocation(Location)

let from_com_transitions com_transitions start =
  let all_trans = List.flatten com_transitions in
  let start_locs = LocationSet.of_list @@ List.map Transition_.src all_trans in

  (* Try to eliminate recursion. When there is a Com_k transition we aim to construct a Com_1 transition by eliminating
   * all targets that do not appear on the left hand side of a rule.
   * However, we need to keep at least on target for each transition, such that the transition itself is not eliminated and it
   * still incurs a cost of 1. *)
  let cleaned_com_k_transitions =
    List.map
      (fun ts ->
        if List.length ts > 1 then
          let arb_trans = List.hd ts in
          let cleaned = List.filter (flip LocationSet.mem start_locs % Transition_.target) ts in
          if List.is_empty cleaned then [arb_trans] else cleaned
          |> tap (fun new_ts -> if List.length new_ts = 1 then Logger.log Logging.(get Program) INFO (fun () ->
              "eliminate_recursion",[ "old_targets", Util.enum_to_string Transition_.to_id_string (List.enum ts)
                                    ; "new_targets", Util.enum_to_string Transition_.to_id_string (List.enum new_ts)]) else ())
        else ts
      )
      com_transitions
  in
  if List.exists (not % Int.equal 1 % List.length) cleaned_com_k_transitions then raise RecursionNotSupported else
    let transs =
      let all = List.flatten cleaned_com_k_transitions in
      let num_arg_vars =
        List.max ~cmp:Int.compare @@ List.map (TransitionLabel_.input_size % Transition_.label) all
      in
      List.map (Transition_.map_label (TransitionLabel_.fill_up_arg_vars_up_to_num num_arg_vars)) all
    in
    from_enum start (List.enum transs)

let rename program =
  let counter: int ref = ref 0 in
  let map = Hashtbl.create 10 in
  let name location =
    Hashtbl.find_option map location
    |> Option.default_delayed (fun () ->
           let new_name = ("l" ^ string_of_int !counter) in
           Hashtbl.add map location new_name;
           counter := !counter + 1;
           Logger.(log Logging.(get Preprocessor) INFO (fun () -> "renaming", ["original", Location.to_string location; "new", new_name]));
           new_name
         )
    |> Location.of_string
  in
  let new_start = name program.start in
  {
    graph = TransitionGraph_.map_vertex name program.graph;
    start = new_start;
  }

(* Prints the program to the file "file.koat" *)
let to_file program file =
  let oc = open_out (file ^ ".koat") in
    Printf.fprintf oc "(GOAL COMPLEXITY) \n(STARTTERM (FUNCTIONSYMBOLS %s))\n(VAR%s)\n(RULES \n%s)"
                    (Location.to_string (start program))
                    (VarSet.fold (fun var str -> str ^ " " ^ Var.to_string ~to_file:true var) (input_vars program) "")
                    (TransitionGraph_.fold_edges_e (fun t str-> str ^ " " ^Transition_.to_file_string t ^ "\n") program.graph "");
    close_out oc
