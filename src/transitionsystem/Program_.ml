open! OurBase
open Constraints
open Formulas

exception RecursionNotSupported

module Make
    (TL : ProgramTypes.TransitionLabel)
    (T : ProgramTypes.Transition
           with type transition_label = TL.t
            and type transition_label_comparator_witness = TL.comparator_witness)
    (G : ProgramTypes.TransitionGraph
           with type transition_label = TL.t
            and type transition_label_comparator_witness = TL.comparator_witness) =
struct
  type transition_label = TL.t
  type transition_label_comparator_witness = TL.comparator_witness
  type transition = Location.t * TL.t * Location.t
  type transition_comparator_witness = T.comparator_witness
  type transition_set = (transition, transition_comparator_witness) Set.t
  type transition_graph = G.t

  module TransitionSet = Transition_.TransitionSetOver (T)
  open GenericProgram_

  type t = (transition_label, transition_label_comparator_witness, transition_graph) GenericProgram_.t

  let start program = program.start
  let graph g = g.graph
  let transitions_from_location program loc = G.succ_e program.graph loc |> TransitionSet.of_list

  let equal equal_graph (program1 : t) (program2 : t) =
    equal_graph program1.graph program2.graph && Location.equal program1.start program2.start


  let equivalent : t -> t -> bool = equal G.equivalent
  let with_pre_cache program = Atomically.run_atomically program.pre_cache
  let map_pre_cache program = Atomically.map_atomically program.pre_cache

  let invalidate_pre_cache_for_transs invalidate_transs program =
    let new_pre_cache =
      map_pre_cache program @@ fun pre_cache ->
      if Hashtbl.is_empty pre_cache then
        pre_cache
      else
        (* copy to ensure immutability from outside view *)
        Hashtbl.copy pre_cache |> tap (fun tbl -> List.iter ~f:(Hashtbl.remove tbl) invalidate_transs)
    in
    { program with pre_cache = new_pre_cache }


  let invalidate_complete_pre_cache program =
    (* TODO  specify hash function? *)
    {
      program with
      pre_cache = Hashtbl.create ~size:(G.nb_edges program.graph) (module T) |> Atomically.create;
    }


  let remove_location program location =
    let affected_transitions =
      (* incoming and outgoing transition are removed *)
      let ingress = G.pred_e program.graph location in
      let outgress = G.succ_e program.graph location in
      let outgreess_of_out_trans = ListMonad.(outgress >>= fun (_, _, l') -> G.succ_e program.graph l') in

      (* transitions following an outgoing transitions might have store the outgoing transition (now removed)  in their pre-cache. So we remove it *)
      ingress @ outgress @ outgreess_of_out_trans
    in
    { program with graph = G.remove_vertex program.graph location }
    |> invalidate_pre_cache_for_transs affected_transitions


  let remove_transition program transition =
    let affected_transitions = transition :: G.succ_e program.graph (T.target transition) in
    { program with graph = G.remove_edge_e program.graph transition }
    |> invalidate_pre_cache_for_transs affected_transitions


  let map_graph f program = invalidate_complete_pre_cache { program with graph = f program.graph }
  let map_transitions f = map_graph (G.map_transitions f)
  let map_labels f = map_transitions (fun (l, t, l') -> (l, f t, l'))
  let locations : t -> LocationSet.t = G.locations % graph
  let transitions = G.transitions % graph
  let simplify_all_guards = ()

  let vars program =
    transitions program |> Set.to_sequence |> Sequence.map ~f:T.label |> Sequence.map ~f:TL.vars
    |> Sequence.fold ~f:Set.union ~init:VarSet.empty


  let input_vars program =
    transitions program |> Set.to_sequence |> Sequence.map ~f:T.label |> Sequence.map ~f:TL.input_vars
    |> Sequence.fold ~f:Set.union ~init:VarSet.empty


  let tmp_vars program = Set.diff (vars program) (input_vars program)

  let from_graph start graph =
    try
      if G.is_empty graph || List.is_empty (G.pred_e graph start) then
        { start; graph; pre_cache = Atomically.create @@ Hashtbl.create ~size:(G.nb_edges graph) (module T) }
      else
        raise (Failure "Transition leading back to the initial location.")
    with
    | Invalid_argument _ ->
        (* G.pred_e throws it if start location does not occur on left side.*)
        let graph = G.empty in
        { start; graph; pre_cache = Atomically.create @@ Hashtbl.create ~size:(G.nb_edges graph) (module T) }


  let from_sequence start = from_graph start % G.mk

  let compute_pre program (l, t, l') =
    let is_satisfiable f =
      try SMT.Z3Solver.satisfiable f with
      | SMT.SMTFailure _ ->
          true (* thrown if solver does not know a solution due to e.g. non-linear arithmetic *)
    in
    l
    |> G.pred_e (graph program)
    |> Sequence.of_list
    |> Sequence.filter ~f:(fun (_, t', _) ->
           TL.chain_guards t' t
           |> is_satisfiable % Formula.mk % Constraint.drop_nonlinear (* such that Z3 uses QF_LIA*))


  let pre_lazy program trans =
    let res = with_pre_cache program @@ fun pre_cache -> Hashtbl.find pre_cache trans in
    match res with
    | Some tset -> Set.to_sequence tset
    | None -> compute_pre program trans


  let pre program trans =
    with_pre_cache program @@ fun pre_cache ->
    match Hashtbl.find pre_cache trans with
    | Some tset -> tset
    | None ->
        let tset = Set.of_sequence (module T) (compute_pre program trans) in
        Hashtbl.add_exn pre_cache ~key:trans ~data:tset;
        tset


  let succ program (l, t, l') =
    G.succ_e (graph program) l' |> List.filter ~f:(fun t' -> Set.mem (pre program t') (l, t, l'))


  let sccs program = G.sccs program.graph |> List.rev (* scc_list is in reverse topological order *)

  let parallel_transitions graph (l, _, l') =
    transitions graph |> Set.filter ~f:(fun (l1, _, l1') -> Location.equal l l1 && Location.equal l' l1')


  let non_trivial_transitions : t -> transition_set = TransitionSet.union_list % sccs
  let add_invariant = ()
  let remove_unsatisfiable_transitions = ()
  let is_initial program trans = Location.(equal program.start (T.src trans))
  let is_initial_location program location = Location.(equal program.start location)

  let to_formatted_string ?(pretty = false) (program : t) =
    let transitions =
      G.fold_edges_e
        (fun t str ->
          str
          @ [
              (if pretty then
                 T.to_string_pretty t
               else
                 T.to_string t);
            ])
        program.graph []
      |> FormattedString.mappend % List.map ~f:FormattedString.mk_str_line
    in
    let locations =
      String.concat ~sep:", " (G.fold_vertex (fun l str -> str @ [ Location.to_string l ]) program.graph [])
    in
    FormattedString.format_append
      ([
         "Start:  " ^ Location.to_string program.start;
         "Program_Vars:  "
         ^ (input_vars program |> VarSet.map_to_list (Var.to_string ~pretty) |> String.concat ~sep:", ");
         "Temp_Vars:  "
         ^ (tmp_vars program |> VarSet.map_to_list (Var.to_string ~pretty) |> String.concat ~sep:", ");
         "Locations:  " ^ locations;
         "Transitions:";
       ]
      |> List.map ~f:FormattedString.mk_str_line
      |> FormattedString.mappend)
      transitions


  let to_string = FormattedString.render_string % to_formatted_string
  let to_simple_string program = G.fold_edges_e (fun t str -> str ^ ", " ^ T.to_string t) program.graph ""

  let entry_transitions program transitions =
    let transitions_set = TransitionSet.of_list transitions in
    let all_possible_pre_transitions = transitions |> List.map ~f:(pre program) |> TransitionSet.union_list in
    Set.diff all_possible_pre_transitions transitions_set |> Set.to_list


  (** All entry transitions of the given transitions.
      These are such transitions, that can occur immediately before one of the transitions, but are not themselves part of the given transitions. *)
  let entry_transitions_with_logger logger (program : t) (transitions : T.t list) : T.t List.t =
    entry_transitions program transitions
    |> tap (fun transitions ->
           Logger.log logger Logger.DEBUG (fun () ->
               ( "entry_transitions",
                 [ ("result", transitions |> Sequence.of_list |> Util.sequence_to_string ~f:T.to_id_string) ]
               )))


  (** All outgoing transitions of the given transitions.
      These are such transitions, that can occur immediately after one of the transitions, but are not themselves part of the given transitions. *)
  let outgoing_transitions logger (program : t) (rank_transitions : T.t list) : T.t List.t =
    rank_transitions |> Sequence.of_list
    |> Sequence.map ~f:(Sequence.of_list % succ program)
    |> Sequence.join
    |> Sequence.filter ~f:(fun r -> rank_transitions |> List.for_all ~f:(not % T.equal r))
    |> TransitionSet.stable_dedup_list % Sequence.to_list
    |> tap (fun transitions ->
           Logger.log logger Logger.DEBUG (fun () ->
               ( "outgoing_transitions",
                 [ ("result", transitions |> Sequence.of_list |> Util.sequence_to_string ~f:T.to_id_string) ]
               )))


  let remove_non_contributors vset = map_labels (TL.remove_non_contributors vset)

  module InternalTest = struct
    let get_pre_cache program = with_pre_cache program @@ fun pre_cache -> Hashtbl.copy pre_cache
    let compute_pre = compute_pre
  end
end

module ClassicalProgram = struct
  include Make (TransitionLabel_) (Transition_.MakeClassical (TransitionLabel_)) (TransitionGraph_)
  module Transition = Transition_.MakeClassical (TransitionLabel_)
  module TransitionGraph = TransitionGraph_

  let add_invariant location invariant = map_graph (TransitionGraph.add_invariant location invariant)

  let simplify_all_guards : t -> t =
    map_transitions (Transition.map_label (TransitionLabel_.map_guard Guard.simplify_guard))


  let remove_unsatisfiable_transitions t = Set.fold ~init:t ~f:remove_transition
end

open GenericProgram_
include ClassicalProgram

let from_com_transitions ?(termination = false) com_transitions start =
  let all_trans = List.join com_transitions in
  let start_locs = Set.of_list (module Location) @@ List.map ~f:Transition_.src all_trans in

  (* Try to eliminate recursion. When there is a Com_k transition we aim to construct a Com_1 transition by eliminating
   * all targets that do not appear on the left hand side of a rule.
   * However, we need to keep at least on target for each transition, such that the transition itself is not eliminated and it
   * still incurs a cost of 1. *)
  if List.is_empty all_trans || (not @@ Set.exists ~f:(Location.equal start) start_locs) then
    from_graph start TransitionGraph_.empty
  else
    let cleaned_com_k_transitions =
      List.map
        ~f:(fun ts ->
          if List.length ts > 1 then
            let arb_trans = List.hd_exn ts in
            let cleaned = List.filter ~f:(Set.mem start_locs % Transition_.target) ts in
            if List.is_empty cleaned then
              [ arb_trans ]
            else
              cleaned
              |> tap (fun new_ts ->
                     if List.length new_ts = 1 then
                       Logger.log
                         Logging.(get Program)
                         INFO
                         (fun () ->
                           ( "eliminate_recursion",
                             [
                               ( "old_targets",
                                 Util.sequence_to_string ~f:Transition_.to_id_string (Sequence.of_list ts) );
                               ( "new_targets",
                                 Util.sequence_to_string ~f:Transition_.to_id_string (Sequence.of_list new_ts)
                               );
                             ] ))
                     else
                       ())
          else
            ts)
        com_transitions
    in
    if (not termination) && List.exists ~f:(not % Int.equal 1 % List.length) cleaned_com_k_transitions then
      raise RecursionNotSupported
    else
      let transs =
        let all = List.join cleaned_com_k_transitions in
        if termination && List.exists ~f:(not % Int.equal 1 % List.length) cleaned_com_k_transitions then
          Logger.log
            Logging.(get Program)
            INFO
            (fun () ->
              ( "eliminate_recursion for termination",
                [
                  ( "new_transitions",
                    Util.sequence_to_string ~f:Transition_.to_id_string (Sequence.of_list all) );
                ] ));
        let num_arg_vars =
          Option.value_exn @@ List.max_elt ~compare:Int.compare
          @@ List.map ~f:(TransitionLabel_.input_size % Transition_.label) all
        in
        List.map ~f:(Transition_.map_label (TransitionLabel_.fill_up_arg_vars_up_to_num num_arg_vars)) all
      in
      from_sequence start (Sequence.of_list transs)


let rename program =
  let counter : int ref = ref 0 in
  let map = Hashtbl.create ~size:10 (module Location) in
  let name location =
    Hashtbl.find map location
    |> Option.value_or_thunk ~default:(fun () ->
           let new_name = "l" ^ string_of_int !counter in
           Hashtbl.add_exn map ~key:location ~data:new_name;
           counter := !counter + 1;
           Logger.(
             log
               Logging.(get Preprocessor)
               INFO
               (fun () -> ("renaming", [ ("original", Location.to_string location); ("new", new_name) ])));
           new_name)
    |> Location.of_string
  in
  let new_start = name program.start in
  {
    graph = TransitionGraph_.map_vertex name program.graph;
    start = new_start;
    pre_cache =
      Atomically.create
      @@ Hashtbl.create ~size:(TransitionGraph_.nb_vertex program.graph) (module Transition_);
  }


(* Prints the program to the file "file.koat" *)
let to_file program file =
  let oc = Stdio.Out_channel.create file in
  Stdio.Out_channel.fprintf oc "(GOAL COMPLEXITY) \n(STARTTERM (FUNCTIONSYMBOLS %s))\n(VAR%s)\n(RULES \n%s)"
    (Location.to_string (start program))
    (Set.fold ~f:(fun str var -> str ^ " " ^ Var.to_string ~to_file:true var) (input_vars program) ~init:"")
    (TransitionGraph_.fold_edges_e
       (fun t str -> str ^ " " ^ Transition_.to_file_string t ^ "\n")
       program.graph "");
  Stdio.Out_channel.close oc
