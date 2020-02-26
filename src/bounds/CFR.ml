open Batteries
open Polynomials
open Atoms
open Constraints
open ProgramTypes
open Formulas

module DjikstraTransitionGraph = Graph.Path.Dijkstra(TransitionGraph)(TransitionGraphWeight(OurInt))

module SCCSetCompare = struct
  type t = TransitionSet.t
  let compare t1 t2 = TransitionSet.compare t1 t2
end

module SCCSet = Set.Make(SCCSetCompare)

let time = ref 0.0

let levelOfCFR = Hashtbl.create 123456;;

let getLevel (t: Transition.t): int = 
  try Hashtbl.find levelOfCFR (Transition.id t)
  with Not_found -> 0

(* TODO *)
let logger = Logging.(get CFR)

(* Collect all non-linear bounds *)
let nonLinearTransitions = ref TransitionSet.empty

(** TODO move to program, All entry transitions of the given transitions.
    These are such transitions, that can occur immediately before one of the transitions, but are not themselves part of the given transitions. *)
let entry_transitions (program: Program.t) (rank_transitions: Transition.t list): Transition.t List.t =
  rank_transitions
  |> List.enum
  |> Enum.map (Program.pre program)
  |> Enum.flatten
  |> Enum.filter (fun r ->
         rank_transitions
         |> List.enum
         |> Enum.for_all (not % Transition.same r)
       )
  |> Enum.uniq_by Transition.same
  |> List.of_enum
  

(* SCCs that contain a non-linear transition, its not guaranteed that they are minimal *)
let nonLinearSCCs (program: Program.t) =
  Program.sccs program
  |> Enum.filter (fun scc -> 
      (!nonLinearTransitions
       |> TransitionSet.exists (fun nonLinearTransition -> 
          scc
          |> TransitionSet.mem nonLinearTransition)))

(* Transforms (non-minimal) TransitionSets (i.e. our SCCs) to TransitionGraphs to apply Djikstra as the next step *)
let getTransitionGraph (nonLinearSCCs: TransitionSet.t list)  =
  nonLinearSCCs
  |> List.map (fun nonLinearSCC -> 
      TransitionGraph.empty
      |> TransitionSet.fold (fun transition graph -> 
            transition
            |> TransitionGraph.add_edge_e graph) nonLinearSCC )

(* Returns the graph which containts an edge from l1 to l2. 
Note that by our previous construction from sccs the graph contains all (parallel) edges l1 -> l2 *)
let findGraph (graphs: TransitionGraph.t list) l1 l2 =
  graphs
  |> List.find (fun graph -> TransitionGraph.mem_edge graph l1 l2)

(* Apply Dijkstra on ProgramGraph and get shortest l2 -> l1 path 
and add transition l1 -> l2 (thus, we get a circle, a minimal SCC) on all SCCs *)
let applyDijkstra (program: Program.t) =
  let nonLinearSCCs = program 
                    |> nonLinearSCCs 
                    |> List.of_enum in
  let graphs = nonLinearSCCs
                |> getTransitionGraph  in
  !nonLinearTransitions
  |> TransitionSet.to_list 
  |> List.map (fun (l1,x,l2) -> 
                      let (path, _) = 
                          DjikstraTransitionGraph.shortest_path (findGraph graphs l1 l2) l2 l1 in
                              [(l1,x,l2)]@path)

(* Add parallel transitions to one SCC *)
let parallelTransitions (program: Program.t) scc = 
  TransitionSet.empty
  |> List.fold_right (fun transition res ->   
        res
        |> TransitionSet.union (transition 
                                |> Program.parallelTransitions program)) scc

(* Constructs minimal SCCs containing a non-linear transition and all parallel edges. *)
let minimalSCCs (program: Program.t) = 
  program 
  |> applyDijkstra
  |> List.map (fun scc -> parallelTransitions program scc)

(* Merges non-disjoint SCCS *)
let minimalDisjointSCCs (original_sccs: ProgramTypes.TransitionSet.t list) =
  original_sccs 
  |> List.map (fun original_scc -> 
          original_scc
          |> List.fold_right 
              (fun scc merged_scc -> 
                if (TransitionSet.disjoint scc merged_scc) then 
                  merged_scc 
                else 
                  (TransitionSet.union scc merged_scc)) original_sccs)
  |> SCCSet.of_list 

(** Counts file and creates always a new file usefull for debugging. *)
let counter = ref 0 

let random = ref 0

(** Creates a file containing irankfinder, applies irankfinder and returns the resulting program. *)
let applyIrankFinder (scc_program: Program.t) = 
  Random.self_init();
  if !random == 0 then (
    random := Random.int 1000000000;
    try Unix.mkdir ("./tmp_" ^ (string_of_int !random)) 0o700 with Unix.Unix_error (Unix.EEXIST, _, _) -> ());
  counter := !counter + 1;
  Program.to_file scc_program ("./tmp_" ^ (string_of_int !random) ^ "/tmp_scc" ^ (string_of_int !counter));
  let delta = Unix.gettimeofday() in
  ignore (Sys.command ("CFRefinement -cfr-it 1 -cfr-call -cfr-head -cfr-john --output-format koat --output-destination ./tmp_" ^ (string_of_int !random) ^ "/tmp --file ./tmp_" ^ (string_of_int !random) ^ "/tmp_scc" ^ (string_of_int !counter) ^ ".koat"));
  "./tmp_" ^ (string_of_int !random) ^ "/tmp/tmp_scc" ^ (string_of_int !counter) ^ "_cfr1.koat"
      |> tap (fun _ -> time := !time +.  (Unix.gettimeofday() -. delta))
      |> Readers.read_input ~rename:false false 
      |> Option.get

(** TODO speed this up. *)
let rename_matching_trans (l_original: Location.t) (l_cfr: Location.t) transitions = 
  transitions
  |> TransitionSet.map (fun (l,g,l') -> 
                                    if Location.equal l l_cfr then
                                      (l_original,g,l')
                                    else 
                                      (l,g,l')) 
  |> TransitionSet.map (fun (l,g,l') -> 
                                    if Location.equal l' l_cfr then
                                     (l,g,l_original)
                                    else 
                                     (l,g,l')) 

let contains s1 s2 =
    let re = Str.regexp_string s2
    in
        try ignore (Str.search_forward re s1 0); true
        with Not_found -> false

(** Finds first location which matches to the entry-location l ->  l' from the original program. 
    We assume that we only get transitions starting in the initial location of cfr program*)
let find_matching_location (l': Location.t) (entry_transitions_cfr: TransitionSet.t) =
  entry_transitions_cfr
  |> TransitionSet.to_list
  |> List.find (fun (_,_,l'_cfr) -> 
                                contains (Location.to_string l'_cfr) ("_" ^ (Location.to_string l') ^ "__")
  )
  |> Tuple3.third


let rename_entry_transition (locations: LocationSet.t) (transitions: TransitionSet.t) =
  transitions
  |> LocationSet.fold (fun location merged_trans -> let l' = find_matching_location location (TransitionSet.filter (fun (l,_,_) ->  Location.equal l (Location.of_string "n_l0" )) transitions) in
                                                             rename_matching_trans location l' transitions) locations 

let apply_cfr (program: Program.t) = 
  let copy_nonLinearTransitions = !nonLinearTransitions
  and initial_location = Program.start program
  and minimalDisjointSCCs = program
                            |> minimalSCCs
                            |> minimalDisjointSCCs 
                            |> SCCSet.filter (fun scc -> (TransitionSet.cardinal scc) > 1)  in
  nonLinearTransitions := TransitionSet.empty;
    program
    |> SCCSet.fold (fun scc merged_program ->  
      (fun sccs -> 
      (* Printf.printf "original program: %s \n" (Program.to_string merged_program); *)
      Logger.log logger Logger.INFO
                               (fun () -> "minimalSCCs", ["non-linear transitions: " ^ (TransitionSet.to_string (TransitionSet.inter copy_nonLinearTransitions scc)), "\n minimalSCC: " ^ (TransitionSet.to_string scc)])) scc;

      let scc_list = TransitionSet.to_list scc in
      let entry_locations = LocationSet.of_list (List.map (fun (_,_,l) -> l) (entry_transitions program scc_list)) in
      let entry_transitions = List.map (fun l -> (initial_location, TransitionLabel.trival (Program.vars program),l)) (LocationSet.to_list entry_locations) in
      let scc_program = Program.from (entry_transitions@scc_list) initial_location in
      let program_cfr  = applyIrankFinder scc_program in
      (** Prepares transitions created by irankfinder to merge. *)
      let map = RenameMap.from_native ((List.init (Program.cardinal_vars program) (fun i -> ("Arg" ^ string_of_int i,"Arg_" ^ string_of_int i))) @
                                      (List.init (Program.cardinal_vars program) (fun i -> ("Arg" ^ string_of_int i,"Arg_" ^ string_of_int i)))) in      
      let transitions_cfr = program_cfr
      |> Program.transitions
      |> rename_entry_transition entry_locations
      |> TransitionSet.filter (fun (l,_,_) -> not (BatString.equal ("n_" ^ (Location.to_string initial_location)) (Location.to_string l)))
      |> TransitionSet.map (fun t -> Transition.rename2 map t) in
      (** Merges irankfinder and original program. *)

      let max_level = TransitionSet.fold (fun t max -> let level = getLevel t in if level > max then level else max) scc 0 in
      TransitionSet.iter (fun t -> Hashtbl.add levelOfCFR (Transition.id t) (max_level + 1)) transitions_cfr;
      TransitionSet.iter (fun t -> Hashtbl.remove levelOfCFR (Transition.id t)) scc;

      merged_program
      |> Program.transitions
      |> TransitionSet.union transitions_cfr
      |> flip TransitionSet.diff scc
      |> TransitionSet.to_list
      |> flip Program.from initial_location) minimalDisjointSCCs
      |> Program.rename