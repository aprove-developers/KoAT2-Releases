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

(** We use this to measure the amount of time spend solving on this part of cfr.*) 
let delta_current_cfr = ref 0.

exception TIMEOUT

(** Table: transition -> amount of times (orginal) transition was involed in CFR. *)
let already_used_cfr = ref TransitionSet.empty

(* TODO *)
let logger = Logging.(get CFR)

(* Collect all non-linear bounds *)
let nonLinearTransitions = ref TransitionSet.empty

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
  ignore (Sys.command ("CFRefinement -cfr-it 1 -cfr-call -cfr-head -cfr-john --output-format koat --output-destination ./tmp_" ^ (string_of_int !random) ^ "/tmp --file ./tmp_" ^ (string_of_int !random) ^ "/tmp_scc" ^ (string_of_int !counter) ^ ".koat"));
  "./tmp_" ^ (string_of_int !random) ^ "/tmp/tmp_scc" ^ (string_of_int !counter) ^ "_cfr1.koat"
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


(** Generates the approximation for the new program_cfr from the one of the original program. *)
let get_appr_cfr (program: Program.t) (program_cfr: Program.t) appr = 
  let unchangend_trans = TransitionSet.inter (Program.transitions program) (Program.transitions program_cfr) in
  (* Printf.printf "TS: \n %s" (TransitionSet.to_string unchangend_trans); *)
  program_cfr
  |> Approximation.create
  |> TrivialTimeBounds.compute program_cfr 
  |> TransitionSet.fold (fun trans appr_cfr ->
                                      let timebound = appr
                                      |> Approximation.time
                                      |> flip TransitionApproximation.get trans and 
                                        costbound = appr
                                      |> Approximation.cost
                                      |> flip TransitionApproximation.get trans in 
                                      VarSet.fold (fun x appr_cfr -> 
                                              let lower_sizebound_x = SizeApproximation.get `Lower (Approximation.size appr) trans x and 
                                                  upper_sizebound_x = SizeApproximation.get `Upper (Approximation.size appr) trans x in
                                                appr_cfr
                                                |> Approximation.add_sizebound `Lower lower_sizebound_x trans x
                                                |> Approximation.add_sizebound `Upper upper_sizebound_x trans x) (Program.vars program) appr_cfr
                                              
                                      |> Approximation.add_timebound timebound trans
                                      |> Approximation.add_costbound costbound trans) unchangend_trans


let apply_cfr (program: Program.t) appr = 
  delta_current_cfr := 0.;
  let copy_nonLinearTransitions = !nonLinearTransitions
  and initial_location = Program.start program
  and minimalDisjointSCCs = program
                            |> minimalSCCs
                            |> minimalDisjointSCCs 
                            |> SCCSet.filter (fun scc -> (TransitionSet.cardinal scc) > 1)  in
  nonLinearTransitions := TransitionSet.empty;
    let program_res = 
    program
    |> SCCSet.fold (fun scc merged_program ->  
      (fun sccs -> 
      (* Printf.printf "original program: %s \n" (Program.to_string merged_program); *)
      Logger.log logger Logger.INFO
                               (fun () -> "minimalSCCs", ["non-linear transitions: " ^ (TransitionSet.to_string (TransitionSet.inter copy_nonLinearTransitions scc)), "\n minimalSCC: " ^ (TransitionSet.to_string scc)])) scc;
      let time_current = Unix.time()
      and scc_list = TransitionSet.to_list scc in
      let entry_locations = LocationSet.of_list (List.map (fun (_,_,l) -> l) (Program.entry_transitions logger program scc_list)) in

      let entry_transitions = List.map (fun l -> (initial_location, TransitionLabel.trival (Program.vars program),l)) (LocationSet.to_list entry_locations) in
      let program_cfr = 
      initial_location
      |> Program.from (entry_transitions@scc_list)
      |> applyIrankFinder

      (** Prepares transitions created by irankfinder to merge. Hier müssen noch die Variablen x' = update(x) verändert werden. *)
      and map = RenameMap.from_native ((List.init (Program.cardinal_vars program) (fun i -> ("Arg" ^ string_of_int i,"Arg_" ^ string_of_int i)))) in      
      let transitions_cfr = program_cfr
      |> Program.transitions
      |> rename_entry_transition entry_locations
      |> TransitionSet.filter (fun (l,_,_) -> not (BatString.equal ("n_" ^ (Location.to_string initial_location)) (Location.to_string l)))
      |> TransitionSet.map (fun t -> Transition.rename2 map t) in

      (* Printf.printf "transitions_cfr: %s" (TransitionSet.to_string transitions_cfr); *)

      (** Ensures that each transition is only used once in a cfr unrolling step. TODO use sets and fix this.  *)
      already_used_cfr := TransitionSet.filter (fun trans -> TransitionSet.mem trans scc) !already_used_cfr;
      already_used_cfr := TransitionSet.union !already_used_cfr transitions_cfr;

      (** Merges irankfinder and original program. *)
      merged_program
      |> Program.transitions
      |> TransitionSet.union transitions_cfr
      |> flip TransitionSet.diff scc
      |> TransitionSet.to_list
      |> flip Program.from initial_location
      (* |> tap (fun x -> Printf.printf "Ist bei diesem Program schon was falsch?\ %s " (Program.to_string x)) *)
      |> tap (fun _ -> delta_current_cfr := !delta_current_cfr +. (Unix.time() -. time_current))) minimalDisjointSCCs
            (* |> tap (fun x -> Printf.printf "Appr: %s \n " (Approximation.to_string x (get_appr_cfr program x appr))) *)
            |> tap (fun x -> Program.to_file x "program_cfr.koat") 
                  |> Program.rename
       in
      (program_res, get_appr_cfr program program_res appr)
