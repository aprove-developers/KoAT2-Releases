open Batteries
open Polynomials
open Atoms
open Constraints
open ProgramTypes
open Formulas
open ApproximationModules

module DjikstraTransitionGraph = Graph.Path.Dijkstra(TransitionGraph)(TransitionGraphWeight(OurInt))

module SCCSetCompare = struct
  type t = TransitionSet.t
  let compare t1 t2 = TransitionSet.compare t1 t2
end

module SCCSet = Set.Make(SCCSetCompare)

(* TODO *)
let logger = Logging.(get CFR)

(** Timeouts *)
(** We use this to measure the amount of time spend solving on the current cfr instance. *)
let delta_current_cfr = ref 0.

let time_current_cfr = ref 0.

let time_cfr = ref 180.

let number_unsolved_trans = ref 0

exception TIMEOUT

(** Set the time which is reserved for this scc: time_left_cfr * #trans_scc/#trans_left. If we have an infinite transition, we set time_current_cfr to inf. (-1). *)
let set_time_current_cfr (scc: TransitionSet.t) appr =
  if !time_cfr >= 0. && (TransitionSet.for_all (fun t -> Approximation.is_time_bounded appr t) scc) then (
    time_cfr := max (!time_cfr -. !delta_current_cfr) 0.;
    delta_current_cfr := 0.;
    time_current_cfr := (float_of_int (TransitionSet.cardinal scc)) /. (float_of_int !number_unsolved_trans) *.  !time_cfr;)
  else (
    delta_current_cfr := 0.;
    time_current_cfr := -1.)

let poll_timeout ?(applied_cfr = true) =
  if applied_cfr && !time_current_cfr < !delta_current_cfr  && !time_current_cfr > 0. then
    raise TIMEOUT



(* SCCs that contain a non-linear transition, its not guaranteed that they are minimal *)
let nonLinearSCCs (nonLinearTransitions: TransitionSet.t) (program: Program.t) =
  Program.sccs program
  |> Enum.filter (fun scc ->
      (nonLinearTransitions
       |> TransitionSet.exists (fun nonLinearTransition ->
          scc
          |> TransitionSet.mem nonLinearTransition)))

(* Transforms (non-minimal) TransitionSets (i.e. our SCCs) to TransitionGraphs to apply Djikstra as the next step. *)
let getTransitionGraph (nonLinearSCCs: TransitionSet.t list)  =
  Logger.log logger Logger.INFO
                                (fun () -> "getTransitionGraph", ["input: " ^ (String.concat "\n" (List.map (TransitionSet.to_string) nonLinearSCCs)), ""]);
  nonLinearSCCs
  |> List.map (fun nonLinearSCC ->
      TransitionGraph.empty
      |> TransitionSet.fold (fun transition graph ->
            transition
            |> TransitionGraph.add_edge_e graph) nonLinearSCC )

(* Apply Dijkstra on ProgramGraph and get shortest l2 -> l1 path
and add transition l1 -> l2 (thus, we get a circle, a minimal SCC) on all SCCs *)
let applyDijkstra (nonLinearTransitions: TransitionSet.t) (program: Program.t) =
  let allNonLinearSCCs = program
                    |> (nonLinearSCCs nonLinearTransitions) in
                    Logger.log logger Logger.INFO
                                (fun () -> "applyDijkstra", ["non-linear sccs: " ^ (Enum.fold (fun str1 str2 -> str1 ^ "\n" ^str2) "" (Enum.map (TransitionSet.to_string) (Enum.clone allNonLinearSCCs))), ""]);
  
  let graphs = allNonLinearSCCs
                |> List.of_enum
                |> getTransitionGraph in
                Logger.log logger Logger.INFO
                                (fun () -> "applyDijkstra", ["non-linear sccs graph: " ^ String.concat "\n" (List.map (TransitionSet.to_string%TransitionGraph.transitions) graphs), "program:" ^ (Program.to_string program)]);
  nonLinearTransitions
  |> TransitionSet.to_list
  |> List.map (fun (l1,x,l2) ->
                      let (path, _) =
                          DjikstraTransitionGraph.shortest_path (graphs
                                                                 |> List.find (fun graph -> TransitionGraph.mem_edge graph l1 l2)) l2 l1 in [(l1,x,l2)]@path)

(* Add transitions which occur in the program and are parallel to a transition from the SCC. *)
let parallelTransitions (program: Program.t) scc =
  TransitionSet.empty
  |> List.fold_right (fun transition res ->
        res
        |> TransitionSet.union (transition
                                |> Program.parallelTransitions program)) scc

(* Constructs minimal SCCs containing a non-linear transition and all parallel edges. *)
let minimalSCCs (nonLinearTransitions: TransitionSet.t) (program: Program.t) =
  program
  |> (applyDijkstra nonLinearTransitions)
  |> List.map (fun scc -> parallelTransitions program scc)

let string x =
  List.fold_right (fun trans str -> str ^ "\n" ^ (TransitionSet.to_string trans)) x ""

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

(** Counts file and creates always a new file useful for debugging. *)
let counter = ref 0

(** To allow several parallel executions of KoAT, we create cfr-output folders with unique names. *)
let uid = ref ""

(** Creates a file containing irankfinder, applies irankfinder and returns the resulting program. *)
let applyIrankFinder (scc_program: Program.t) =
  if !uid == "" then (
    uid := (string_of_int (Unix.getpid ())) ^ "_" ^ (string_of_float (Unix. gettimeofday ()));
    try Unix.mkdir ("./tmp_" ^ !uid) 0o700 with Unix.Unix_error (Unix.EEXIST, _, _) -> ());
  counter := !counter + 1;
  Program.to_file scc_program ("./tmp_" ^ !uid ^ "/tmp_scc" ^ (string_of_int !counter));
  ignore (Sys.command ("CFRefinement -cfr-it 1 -cfr-call -cfr-head -cfr-head-deep --no-unfold all --output-format koat --output-destination ./tmp_"
               ^ !uid
               ^ "/tmp --file ./tmp_"
               ^ !uid
               ^ "/tmp_scc"
               ^ (string_of_int !counter)
               ^ ".koat"));
  "./tmp_" ^ !uid ^ "/tmp/tmp_scc" ^ (string_of_int !counter) ^ "_cfr1.koat"
      |> Readers.read_input ~rename:false false
      |> Option.get

(** TODO speed this up. -------------------------------------  *)
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

(** Finds first location which matches to the entry-location l ->  l' from the original program.
    We assume that we only get transitions starting in the initial location of cfr program*)
let find_matching_locations (l': Location.t) (entry_transitions_cfr: TransitionSet.t) =
  entry_transitions_cfr
  |> TransitionSet.enum
  |> Enum.filter (fun (_,_,l'_cfr) ->
                            Util.contains (Location.to_string l'_cfr) ("n_" ^ (Location.to_string l') ^ "__")
  )
  |> Enum.map (fun (_,_,l') -> l')
  |> LocationSet.of_enum



let rename_entry_transition (entry_locations: LocationSet.t) (initial_location: Location.t) (transitions: TransitionSet.t) =
  transitions
  |> LocationSet.fold (fun location merged_trans ->
    let all_l' = find_matching_locations location
        (TransitionSet.filter
          (fun (l,_,_) ->
            Location.equal l (Location.of_string ("n_" ^ (Location.to_string initial_location))))
            transitions) in
          merged_trans
          |> LocationSet.fold (fun l' -> rename_matching_trans location l') all_l') entry_locations
(* ------------------------------------- *)

(** Adds original outgoing transitions, i.e.,  program and program_cfr are path equivalent. *)
let outgoing_transitions (outgoing_trans: Transition.t list) (scc_cfr: TransitionSet.t) =
  let locations_cfr = TransitionSet.fold (fun (l,_,l') locations -> locations
                                                                       |> LocationSet.add l
                                                                       |> LocationSet.add l') scc_cfr LocationSet.empty in
  TransitionSet.empty
  |> List.fold_right (fun (l,t,l') set -> locations_cfr
                                          |> LocationSet.filter (fun l_cfr -> Util.contains (Location.to_string l_cfr) ("_" ^ (Location.to_string l) ^ "__"))
                                          |> LocationSet.enum
                                          |> TransitionSet.create (function l_cfr ->(l_cfr, TransitionLabel.fresh_id t,l'))
                                          |> TransitionSet.union set) outgoing_trans


(** Generates the approximation for the new program_cfr from the one of the original program. *)
let get_appr_cfr (program: Program.t) (program_cfr: Program.t) appr =
  let unchangend_trans = TransitionSet.inter (Program.transitions program) (Program.transitions program_cfr) in
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
                                              let sizebound_x = SizeApproximation.get (Approximation.size appr) trans x in
                                                appr_cfr
                                                |> Approximation.add_sizebound sizebound_x trans x) (Program.vars program) appr_cfr

                                      |> Approximation.add_timebound timebound trans
                                      |> Approximation.add_costbound costbound trans) unchangend_trans

let apply_cfr (nonLinearTransitions: TransitionSet.t) (already_used:IDSet.t) (program: Program.t) appr =
  delta_current_cfr := 0.;
  let initial_location = Program.start program
  and minimalDisjointSCCs = program
                            |> (minimalSCCs nonLinearTransitions)
                            |> minimalDisjointSCCs
                            in
  if SCCSet.is_empty minimalDisjointSCCs then
    None
  else (
    let f_iteration = 
      fun scc (merged_program,already_used_trans) ->
      (fun sccs ->
      Logger.log logger Logger.INFO
                                (fun () -> "minimalSCCs", ["non-linear transitions: " ^ (TransitionSet.to_string (TransitionSet.inter nonLinearTransitions scc)), "\n minimalSCC: " ^ (TransitionSet.to_string scc)])) scc;
      let time_current = Unix.gettimeofday()
      and scc_list = TransitionSet.to_list scc in
      let entry_locations = LocationSet.of_list (List.map (fun (_,_,l) -> l) (Program.entry_transitions logger program scc_list)) in

      let entry_transitions = List.map (fun l -> (initial_location, TransitionLabel.trival (Program.input_vars program),l)) (LocationSet.to_list entry_locations) in
      let program_cfr =
      initial_location
      |> Program.from (entry_transitions@scc_list)
      |> applyIrankFinder

      (** Prepares transitions created by irankfinder to merge. Hier müssen noch die Variablen x' = update(x) verändert werden. *)
      and map = RenameMap.from_native ((List.init (Program.cardinal_vars program) (fun i -> ("Arg" ^ string_of_int i,"Arg_" ^ string_of_int i)))) in
      let transitions_cfr = program_cfr
      |> Program.transitions
      |> rename_entry_transition entry_locations initial_location
      |> TransitionSet.filter (fun (l,_,_) -> not (BatString.equal ("n_" ^ (Location.to_string initial_location)) (Location.to_string l)))
      |> TransitionSet.map (fun t -> Transition.rename2 map t) in

      (** Ensures that each transition is only used once in a cfr unrolling step. TODO use sets and fix this.  *)
      let
      already_used_cfr = IDSet.union
        (IDSet.union already_used_trans (IDSet.of_enum (Enum.map (fun trans -> Transition.id trans) (TransitionSet.enum transitions_cfr))))
        (IDSet.of_enum (Enum.map (fun t -> Transition.id t) (TransitionSet.enum nonLinearTransitions)))
      and
      (** Merges irankfinder and original program. *)
      processed_program =
      merged_program
      |> Program.transitions
      |> TransitionSet.union transitions_cfr
      |> TransitionSet.union (outgoing_transitions (Program.outgoing_transitions logger program scc_list) transitions_cfr)
      |> flip TransitionSet.diff scc
      |> TransitionSet.to_list
      |> flip Program.from initial_location
      |> tap (fun _ -> delta_current_cfr := !delta_current_cfr +. (Unix.gettimeofday() -. time_current))
      |> tap (fun _ -> poll_timeout ~applied_cfr:true)
      in
      (processed_program,already_used_cfr)
    in
    let (program_res,already_used_cfr_res) =
    (program,IDSet.empty)
    |> SCCSet.fold (f_iteration ) minimalDisjointSCCs
      (* |> Program.rename *)
      in
      Option.some (program_res, (get_appr_cfr program program_res appr), already_used_cfr_res))
