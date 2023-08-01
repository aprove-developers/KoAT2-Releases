open OurBase
open Constraints
open Formulas
open Util

exception RecursionNotSupported

module GenericProgram = struct
  type ('a, 'b) t = { start: 'a; graph: 'b }
end

module Make(TL: ProgramTypes.TransitionLabel)
           (T: ProgramTypes.Transition with type transition_label = TL.t)
           (L: ProgramTypes.Location with type t = T.location)
           (G: ProgramTypes.TransitionGraph with type location = L.t
                                             and type location_set = Location.LocationSetOver(L).t
                                             and type transition_label = TL.t
                                             and type transition_set = Transition_.TransitionSetOver(T)(L).t) =
struct
  type location = L.t
  type transition_label = TL.t
  type transition = L.t * TL.t * L.t
  type location_set = Location.LocationSetOver(L).t
  type transition_set = Transition_.TransitionSetOver(T)(L).t
  type transition_graph = G.t

  module TransitionSet = Transition_.TransitionSetOver(T)(L)

  open GenericProgram
  type t = (location, transition_graph) GenericProgram.t

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
    Set.fold ~f:remove_transition transitions ~init:program

  let map_graph f program =
    { program with graph = f program.graph }

  let map_transitions f = map_graph (G.map_transitions f)

  let map_labels f = map_transitions (fun(l,t,l') -> l,f t,l')

  let locations: t -> location_set = G.locations % graph

  let transitions =
    G.transitions % graph

  let simplify_all_guards: t -> t =
    map_transitions (T.map_label (TL.map_guard Guard.simplify_guard))

  let vars program =
    transitions program
    |> Set.to_sequence
    |> Sequence.map ~f:T.label
    |> Sequence.map ~f:TL.vars
    |> Sequence.fold ~f:Set.union ~init:VarSet.empty

  let input_vars program =
    transitions program
    |> Set.to_sequence
    |> Sequence.map ~f:T.label
    |> Sequence.map ~f:TL.input_vars
    |> Sequence.fold ~f:Set.union ~init:VarSet.empty

  let tmp_vars =
    fun program -> Set.diff (vars program) (input_vars program)

  let from_graph start graph =
    if G.is_empty graph || List.is_empty (G.pred_e graph start) then
      { start; graph; }
    else raise (Failure "Transition leading back to the initial location.")

  let from_sequence start =
    from_graph start % G.mk

  let pre program (l,t,_) =
    let is_satisfiable f =
      try SMT.Z3Solver.satisfiable f
      with SMT.SMTFailure _ -> true (* thrown if solver does not know a solution due to e.g. non-linear arithmetic *)
    in
    l
    |> G.pred_e (graph program)
    |> Sequence.of_list
    |> Sequence.filter ~f:(fun (_,t',_) ->
           TL.chain_guards t' t
           |> is_satisfiable % Formula.mk % Constraint.drop_nonlinear (* such that Z3 uses QF_LIA*)
         )

  let pre_cache: (int, TransitionSet.t) Hashtbl.t = Hashtbl.create ~size:10 (module Int)
  let pre_transitionset_cached program = Util.memoize_base_hashtbl pre_cache ~extractor:T.id (TransitionSet.of_sequence % pre program)
  let reset_pre_cache () = Hashtbl.clear pre_cache

  let succ program (_,t,l') =
    G.succ_e (graph program) l'
    |> Sequence.of_list
    |> Sequence.filter ~f:(fun (_,t',_) ->
           TL.chain_guards t t'
           |> Formula.mk
           |> SMT.Z3Solver.satisfiable
         )

  let sccs program =
    G.sccs program.graph
    |> List.rev (* scc_list is in reverse topological order *)

  let parallel_transitions graph (l,_,l') =
    transitions graph
      |> Set.filter ~f:(fun (l1,_,l1') ->  L.equal l l1 && L.equal l' l1')

  let non_trivial_transitions: t -> transition_set =
    TransitionSet.union_list % sccs

  let add_invariant location invariant =
    map_graph (G.add_invariant location invariant)

  let is_initial program trans =
    L.(equal (program.start) (T.src trans))

  let is_initial_location program location =
    L.(equal (program.start) location)

  let to_formatted_string ?(pretty=false) program =
    let transitions =
      G.fold_edges_e (fun t str -> str @ [if pretty then T.to_string_pretty t else T.to_string t]) program.graph []
      |> FormattedString.mappend % List.map ~f:FormattedString.mk_str_line
    in
    let locations = String.concat ~sep:", " (G.fold_vertex (fun l str -> str @ [(L.to_string l)]) program.graph []) in
    FormattedString.format_append (
      [
        "Start:  "^L.to_string program.start;
        "Program_Vars:  "^(input_vars program |> VarSet.map_to_list (Var.to_string ~pretty) |> String.concat ~sep:", ");
        "Temp_Vars:  "^(tmp_vars program |> VarSet.map_to_list (Var.to_string ~pretty) |> String.concat ~sep:", ");
        "Locations:  "^locations;
        "Transitions:";
      ] |> List.map ~f:(FormattedString.mk_str_line) |> FormattedString.mappend)
    transitions

  let to_string = FormattedString.render_string % to_formatted_string

  let to_simple_string program =
    G.fold_edges_e (fun t str -> str ^ ", " ^ T.to_string t) program.graph ""

  (** All entry transitions of the given transitions.
      These are such transitions, that can occur immediately before one of the transitions, but are not themselves part of the given transitions. *)
  let entry_transitions logger (program: t) (rank_transitions: T.t list): T.t List.t =
    rank_transitions
    |> Sequence.of_list
    |> Sequence.map ~f:(pre program)
    |> Sequence.join
    |> Sequence.filter ~f:(fun r ->
           rank_transitions
           |> List.for_all ~f:(not % T.equal r)
         )
    |> TransitionSet.stable_dedup_list % Sequence.to_list
    |> tap (fun transitions -> Logger.log logger Logger.DEBUG
                                 (fun () -> "entry_transitions", ["result", transitions |> Sequence.of_list |> Util.sequence_to_string ~f:T.to_id_string]))

  (** All outgoing transitions of the given transitions.
      These are such transitions, that can occur immediately after one of the transitions, but are not themselves part of the given transitions. *)
  let outgoing_transitions logger (program: t) (rank_transitions: T.t list): T.t List.t =
    rank_transitions
    |> Sequence.of_list
    |> Sequence.map ~f:(succ program)
    |> Sequence.join
    |> Sequence.filter ~f:(fun r ->
           rank_transitions
           |> List.for_all ~f:(not % T.equal r)
         )
    |> Sequence.to_list
    |> List.dedup_and_sort ~compare:T.compare
    |> tap (fun transitions -> Logger.log logger Logger.DEBUG
                                 (fun () -> "outgoing_transitions", ["result", transitions |> Sequence.of_list |> Util.sequence_to_string ~f:T.to_id_string]))

  let remove_non_contributors vset = map_labels (TL.remove_non_contributors vset)

end

module ProgramOverLocation(L: ProgramTypes.Location) =
  Make(TransitionLabel_) (Transition_.TransitionOver(TransitionLabel_)(L)) (L)
      (TransitionGraph_.TransitionGraphOverLocation(L))

open GenericProgram
include ProgramOverLocation(Location)

let from_com_transitions com_transitions start =
  let all_trans = List.join com_transitions in
  let start_locs = Set.of_list (module Location) @@ List.map ~f:Transition_.src all_trans in

  (* Try to eliminate recursion. When there is a Com_k transition we aim to construct a Com_1 transition by eliminating
   * all targets that do not appear on the left hand side of a rule.
   * However, we need to keep at least on target for each transition, such that the transition itself is not eliminated and it
   * still incurs a cost of 1. *)
  if List.is_empty all_trans then
    from_graph start (TransitionGraph_.empty)
  else
    let cleaned_com_k_transitions =
      List.map
        ~f:(fun ts ->
          if List.length ts > 1 then
            let arb_trans = List.hd_exn ts in
            let cleaned = List.filter ~f:(Set.mem start_locs % Transition_.target) ts in
            if List.is_empty cleaned then [arb_trans] else cleaned
            |> tap (fun new_ts -> if List.length new_ts = 1 then Logger.log Logging.(get Program) INFO (fun () ->
                "eliminate_recursion",[ "old_targets", Util.sequence_to_string ~f:Transition_.to_id_string (Sequence.of_list ts)
                                      ; "new_targets", Util.sequence_to_string ~f:Transition_.to_id_string (Sequence.of_list new_ts)]) else ())
          else ts
        )
        com_transitions
    in
    if List.exists ~f:(not % Int.equal 1 % List.length) cleaned_com_k_transitions then raise RecursionNotSupported else
      let transs =
        let all = List.join cleaned_com_k_transitions in
        let num_arg_vars =
          Option.value_exn @@ List.max_elt ~compare:Int.compare @@ List.map ~f:(TransitionLabel_.input_size % Transition_.label) all
        in
        List.map ~f:(Transition_.map_label (TransitionLabel_.fill_up_arg_vars_up_to_num num_arg_vars)) all
      in
      from_sequence start (Sequence.of_list transs)

let rename program =
  let counter: int ref = ref 0 in
  let map = Hashtbl.create ~size:10 (module Location) in
  let name location =
    Hashtbl.find map location
    |> Option.value_or_thunk ~default:(fun () ->
           let new_name = ("l" ^ string_of_int !counter) in
           Hashtbl.add_exn map ~key:location ~data:new_name;
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
  let oc = Stdio.Out_channel.create (file ^ ".koat") in
  Stdio.Out_channel.fprintf oc "(GOAL COMPLEXITY) \n(STARTTERM (FUNCTIONSYMBOLS %s))\n(VAR%s)\n(RULES \n%s)"
                  (Location.to_string (start program))
                  (Set.fold ~f:(fun str var -> str ^ " " ^ Var.to_string ~to_file:true var) (input_vars program) ~init:"")
                  (TransitionGraph_.fold_edges_e (fun t str-> str ^ " " ^Transition_.to_file_string t ^ "\n") program.graph "");
  Stdio.Out_channel.close oc
