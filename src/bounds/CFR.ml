open Batteries
open Polynomials
open Atoms
open Constraints
open ProgramTypes
open Formulas

module DjikstraTransitionGraph = Graph.Path.Dijkstra(TransitionGraph)(TransitionGraphWeight(OurInt))

module SetOfTransitionSetsCompare = struct
  type t = TransitionSet.t
  let compare t1 t2 = TransitionSet.compare t1 t2
end

module SetOfTransitionSets = Set.Make(SetOfTransitionSetsCompare)

(* TODO *)
let logger = Logging.(get CFR)

(* Collect all non-linear bounds *)
let nonLinearTransitions = ref TransitionSet.empty

(** TODO All entry transitions of the given transitions.
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
  

(*Those SCCs are not minimal containing transition u -> v *)
let nonLinearSCCs (program: Program.t) =
  Program.sccs program
  |> Enum.filter (fun scc -> (TransitionSet.exists (fun nonLinearTransition -> TransitionSet.mem nonLinearTransition scc) !nonLinearTransitions))

(*Transform (non-minimal) TransitionSets (i.e. our SCCs) to a ProgramGraph*)
let getTransitionGraph (nonLinearSCCs: TransitionSet.t list) (l1,x,l2)  =
  TransitionGraph.empty
  |> TransitionSet.fold (fun t graph -> TransitionGraph.add_edge_e graph t) (List.find (fun scc -> TransitionSet.mem (l1,x,l2) scc) nonLinearSCCs)

(* Apply Dijkstra on ProgramGraph and get shortest v -> u path and add transition v -> u (thus, we get a circle, a minimal SCC)*)
let applyDijkstra (program: Program.t) =
  let nonLinearSCCs = List.of_enum (nonLinearSCCs program) in
  let nonLinearTransitions =  TransitionSet.to_list !nonLinearTransitions in
  List.map (fun (l1,x,l2) -> let (path, _) = DjikstraTransitionGraph.shortest_path (getTransitionGraph nonLinearSCCs (l1,x,l2)) l2 l1 in
                              List.append [(l1,x,l2)] path) nonLinearTransitions

(* Add parallel transitions for one scc*)
let parallelTransitions (program: Program.t) (scc: ProgramTypes.TransitionGraph.E.t list) = 
  TransitionSet.empty
  |> List.fold_right (fun transition x -> TransitionSet.fold (fun parallelTransition x1 -> TransitionSet.add parallelTransition x1) (Program.parallelTransitions program transition) x) scc

let minimalSCCs (program: Program.t) = 
  (applyDijkstra program)
  |> List.map (fun scc -> parallelTransitions program scc)

let minimalDisjointSCCs (origin_sccs: ProgramTypes.TransitionSet.t list) =
  SetOfTransitionSets.of_list (List.map (fun origin_scc -> List.fold_right (fun origin_scc1 new_scc -> if (TransitionSet.disjoint origin_scc1 new_scc) then new_scc else (TransitionSet.union origin_scc1 new_scc)) origin_sccs origin_scc) origin_sccs)

let counter = ref 0 

let apply_cfr (program: Program.t) = 
  let initial_location = Program.start program in
  let minimalDisjointSCCs = minimalDisjointSCCs (minimalSCCs program) in
    (* SetOfTransitionSets.iter (fun scc -> Printf.printf "%s\n------------------------------------------------\n" (TransitionSet.to_string scc)) minimalDisjointSCCs; *)
    minimalDisjointSCCs
    |> SetOfTransitionSets.iter (fun scc ->  
      (fun sccs -> Logger.log logger Logger.INFO
                               (fun () -> "minimalSCCs", ["non-linear transitions: " ^ (TransitionSet.to_string (TransitionSet.inter !nonLinearTransitions scc)), "\n minimalSCC: " ^ (TransitionSet.to_string scc)])) scc;
      let scc_list = TransitionSet.to_list scc in
      let entry_transitions = List.map (fun (l,t,l') -> (initial_location,t,l')) (entry_transitions program scc_list) in
      let scc_program = Program.from (List.append entry_transitions scc_list) initial_location in
        ignore (try Unix.mkdir "tmp" 0o700 with Unix.Unix_error (Unix.EEXIST, _, _) -> ());
        Program.to_file scc_program ("./tmp/tmp_scc" ^ (string_of_int !counter));
        ignore (Sys.command ("CFRefinement -cfr-it 1 -cfr-call -cfr-head -cfr-john --output-format koat --output-destination ./tmp/tmp --file ./tmp/tmp_scc" ^ (string_of_int !counter) ^ ".koat"));
         ignore ("./tmp/tmp/tmp_scc" ^ (string_of_int !counter) ^ "_cfr1.koat"
          |> Readers.read_input ~rename:false false 
          |> Option.map (fun program_scc -> (program_scc, Approximation.create program_scc))); 
        counter := !counter + 1;)
