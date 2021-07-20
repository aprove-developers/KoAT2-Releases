open Batteries
open Constraints
open Formulas
open ProgramTypes
open Util

type t = {
    graph: TransitionGraph.t;
    start: Location.t;
  }

let start program = program.start

let graph g = g.graph

let equal equal_graph program1 program2 =
  equal_graph program1.graph program2.graph
  && Location.equal program1.start program2.start

let equivalent =
  equal TransitionGraph.equivalent

let add_locations locations graph =
  locations
  |> Enum.map (fun l -> fun gr -> TransitionGraph.add_vertex gr l)
  |> Enum.fold (fun gr adder -> adder gr) graph

let add_transitions transitions graph =
  transitions
  |> Enum.map (fun t -> fun gr -> TransitionGraph.add_edge_e gr t)
  |> Enum.fold (fun gr adder -> adder gr) graph

let remove_location program location =
  { program with graph = TransitionGraph.remove_vertex program.graph location }

let remove_transition program transition =
  { program with graph = TransitionGraph.remove_edge_e program.graph transition }

(* Removes the transitions from a certain transitionset to a program *)
let remove_TransitionSet (transitions:  TransitionSet.t) (program: t)  =
  program
  |> TransitionSet.fold (fun transition resulting_program  ->
                                transition
                                |> remove_transition resulting_program) transitions

let map_graph f program =
  { program with graph = f program.graph }

let locations transitions =
  transitions
  |> Enum.concat_map (fun (l,_,l') -> List.enum [l; l'])
  |> Enum.uniq_by Location.equal

let mk transitions =
  let locations = locations (Enum.clone transitions) in
  TransitionGraph.empty
  |> add_locations locations
  |> add_transitions transitions

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
    graph = TransitionGraph.map_vertex name program.graph;
    start = new_start;
  }

let from transitions start =
  transitions
  |> fun transitions ->
     if transitions |> List.map Transition.target |> List.mem_cmp Location.compare start then
       raise (Failure "Transition leading back to the initial location.")
     else
       {
         graph = mk (List.enum transitions);
         start = start;
       }

let transitions =
  TransitionGraph.transitions % graph

let vars program =
  program
  |> transitions
  |> TransitionSet.enum
  |> Enum.map Transition.label
  |> Enum.map TransitionLabel.vars
  |> Enum.fold VarSet.union VarSet.empty

let input_vars program =
  program
  |> transitions
  |> TransitionSet.enum
  |> Enum.map Transition.label
  |> Enum.map TransitionLabel.input_vars
  |> Enum.fold VarSet.union VarSet.empty

let temp_vars =
  fun program -> VarSet.diff (vars program) (input_vars program)

let cardinal_vars program =
  VarSet.cardinal (vars program)

let pre program (l,t,_) =
  let is_satisfiable f =
    try SMT.Z3Opt.satisfiable f
    with SMT.SMTFailure _ -> true (* thrown if solver does not know a solution due to e.g. non-linear arithmetic *)
  in
  l
  |> TransitionGraph.pred_e (graph program)
  |> List.enum
  |> Enum.filter (fun (_,t',_) ->
         TransitionLabel.append t' t
         |> TransitionLabel.guard
         |> is_satisfiable % Formula.mk
       )

let pre_cache: (int, TransitionSet.t) Hashtbl.t = Hashtbl.create 10
let pre_transitionset_cached program = Util.memoize pre_cache ~extractor:Transition.id (TransitionSet.of_enum % pre program)
let reset_pre_cache () = Hashtbl.clear pre_cache

let succ program (_,t,l') =
  l'
  |> TransitionGraph.succ_e (graph program)
  |> List.enum
  |> Enum.filter (fun (_,t',_) ->
         TransitionLabel.append t t'
         |> TransitionLabel.guard
         |> Formula.mk
         |> SMT.Z3Opt.satisfiable
       )

let sccs program =
  let module SCC = Graph.Components.Make(TransitionGraph) in
  program.graph
  |> SCC.scc_list
  |> List.rev
  |> List.enum
  |> Enum.map (TransitionGraph.loc_transitions program.graph)
  |> Enum.filter (not % TransitionSet.is_empty)

let cardinal_trans_scc program =
  Enum.fold (fun counter scc -> let cardinal = (TransitionSet.cardinal scc) in counter + if cardinal > 1 then cardinal else 0) 0 (sccs program)

let parallelTransitions graph (l,_,l') =
  transitions graph
    |> TransitionSet.filter (fun (l1,_,l1') ->  Location.equal l l1 && Location.equal l' l1')

let non_trivial_transitions =
  Enum.fold TransitionSet.union TransitionSet.empty % sccs

let add_invariant location invariant =
  map_graph (TransitionGraph.add_invariant location invariant)

let is_initial program trans =
  Location.(equal (program.start) (Transition.src trans))

let is_initial_location program location =
  Location.(equal (program.start) location)

let to_formatted_string program =
  let transitions =
    TransitionGraph.fold_edges_e (fun t str -> str @ [(Transition.to_string t)]) program.graph []
    |> FormattedString.mappend % List.map FormattedString.mk_str_line
  in
  let locations = String.concat ", " (TransitionGraph.fold_vertex (fun l str -> str @ [(Location.to_string l)]) program.graph []) in
  FormattedString.format_append (
    [
      "Start:  "^Location.to_string program.start;
      "Program_Vars:  "^(program |> input_vars |> VarSet.map_to_list Var.to_string |> String.concat ", ");
      "Temp_Vars:  "^(program |> temp_vars |> VarSet.map_to_list Var.to_string |> String.concat ", ");
      "Locations:  "^locations;
      "Transitions:";
    ] |> List.map (FormattedString.mk_str_line) |> FormattedString.mappend)
  transitions

let to_string = FormattedString.render_string % to_formatted_string

let to_simple_string program =
  TransitionGraph.fold_edges_e (fun t str -> str ^ ", " ^ Transition.to_string t) program.graph ""

(* Prints the program to the file "file.koat" *)
let to_file program file =
  let oc = open_out (file ^ ".koat") in
    Printf.fprintf oc "(GOAL COMPLEXITY) \n(STARTTERM (FUNCTIONSYMBOLS %s))\n(VAR%s)\n(RULES \n%s)"
                    (Location.to_string (start program))
                    (VarSet.fold (fun var str -> str ^ " " ^ Var.to_string ~to_file:true var) (input_vars program) "")
                    (TransitionGraph.fold_edges_e (fun t str-> str ^ " " ^(Transition.to_string ~to_file:true t) ^ "\n") program.graph "");
    close_out oc

(** All entry transitions of the given transitions.
    These are such transitions, that can occur immediately before one of the transitions, but are not themselves part of the given transitions. *)
let entry_transitions logger (program: t) (rank_transitions: Transition.t list): Transition.t List.t =
  rank_transitions
  |> List.enum
  |> Enum.map (pre program)
  |> Enum.flatten
  |> Enum.filter (fun r ->
         rank_transitions
         |> List.enum
         |> Enum.for_all (not % Transition.same r)
       )
  |> Enum.uniq_by Transition.same
  |> List.of_enum
  |> tap (fun transitions -> Logger.log logger Logger.DEBUG
                               (fun () -> "entry_transitions", ["result", transitions |> List.enum |> Util.enum_to_string Transition.to_id_string]))

(** All outgoing transitions of the given transitions.
    These are such transitions, that can occur immediately after one of the transitions, but are not themselves part of the given transitions. *)
let outgoing_transitions logger (program: t) (rank_transitions: Transition.t list): Transition.t List.t =
  rank_transitions
  |> List.enum
  |> Enum.map (succ program)
  |> Enum.flatten
  |> Enum.filter (fun r ->
         rank_transitions
         |> List.enum
         |> Enum.for_all (not % Transition.same r)
       )
  |> Enum.uniq_by Transition.same
  |> List.of_enum
  |> tap (fun transitions -> Logger.log logger Logger.DEBUG
                               (fun () -> "outgoing_transitions", ["result", transitions |> List.enum |> Util.enum_to_string Transition.to_id_string]))
