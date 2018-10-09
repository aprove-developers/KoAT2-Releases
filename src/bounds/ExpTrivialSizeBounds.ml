open Batteries
open BoundsInst
open Formulas
open ProgramTypes
   
let logger = Logging.(get Size)

type kind = [ `Lower | `Upper ] [@@deriving show]

module Traverse = Graph.Traverse.Bfs(TransitionGraph)
module GTRV = RVGTypes.Make_RV(struct
                                 include GeneralTransition
                                 let compare_equivalent = GeneralTransition.compare
                                 let compare_same = GeneralTransition.compare
                                 let equivalent = GeneralTransition.same
                               end)
           
(** Returns the maximum of all incoming sizebounds applied to the local sizebound.
    Corresponds to 'SizeBounds for trivial SCCs':
    S'(alpha) = max(S_l(alpha)(S(t',v_1),...,S(t',v_n)) for all t' in pre(t)) *)
let incoming_bound kind program get_sizebound lsb gt (pr_func: Location.t -> float option) =
  let execute () =
    let substitute_with_prevalues gt' = 
      ExpLocalSizeBound.as_substituted_bound (fun kind v -> get_sizebound kind gt' v (GeneralTransition.start gt')) lsb
    in
    let pre_gts = 
      gt
      |> GeneralTransition.transitions
      |> TransitionSet.to_list
      |> List.hd
      |> Program.pre program
      |> TransitionSet.of_enum
      |> GeneralTransitionSet.from_transitionset
    in
    let substitute_with_pr_prevalues gtset (pr_func_unboxed: Location.t -> float option) = 
      let pre_with_pr =
        GeneralTransitionSet.to_list gtset
        |> List.map (fun gt' -> pr_func_unboxed (GeneralTransition.start gt')
                                |> Option.map (fun pr -> (pr,gt')))
        |> Util.option_sequence
      in
      match pre_with_pr with
        | None -> None
        | Some pres_with_prs ->
            Some (ExpLocalSizeBound.as_substituted_bound 
              (fun kind v -> pres_with_prs
                             |> (List.fold_left 
                                     (fun b (pr,gt') -> RealBound.( b + ((of_constant (OurFloat.of_float pr)) * (get_sizebound kind gt' v
                                     (GeneralTransition.start gt'))) ) ) 
                                     RealBound.zero
                                  )
                                ) lsb)
    in
    let pr_prevalues = substitute_with_pr_prevalues pre_gts pr_func in
    match pr_prevalues with
      | None -> 
          pre_gts
          |> GeneralTransitionSet.enum
          |> Enum.map substitute_with_prevalues
          |> (match kind with
              | `Lower -> RealBound.minimum
              | `Upper -> RealBound.maximum)
      | Some (bound) ->
          bound
  in Logger.with_log logger Logger.DEBUG
                     (fun () -> "compute highest incoming bound", ["lsb", (RealBound.to_string % ExpLocalSizeBound.as_bound) lsb;
                                                                   "transition", GeneralTransition.to_string gt])
                  ~result:RealBound.to_string
                  execute


let every_path_enters_loc graph start_location middle_location target_location : bool=
  let graph_without_middle = 
    graph
    |> (flip TransitionGraph.remove_vertex) middle_location 
  in
  let reachable_locations = 
    Traverse.fold_component LocationSet.add LocationSet.empty graph_without_middle (start_location)
  in
  (* If we cut out middle location and in the resulting graph target_location is still reachable then there is also path 
   * in the original graph reaching target_location without entering middle_location*)
  not (LocationSet.mem target_location reachable_locations)

let gt_outgoing tset loc = 
  GeneralTransitionSet.from_transitionset tset
  |> GeneralTransitionSet.filter (Location.equal loc % GeneralTransition.start) 

let build_subgraph (graph : TransitionGraph.t) (loc : Location.t) (cut_location : Location.t) = 
  let cut_graph = TransitionGraph.remove_vertex graph cut_location in
  let reachable_locations = 
    Traverse.fold_component LocationSet.add LocationSet.empty cut_graph loc
  in
  let subgraph_transitions = 
    TransitionGraph.transitions graph
    |> TransitionSet.filter ((flip LocationSet.mem) reachable_locations % Transition.src)
  in
  LocationSet.fold (fun l g -> TransitionGraph.add_vertex g l) reachable_locations TransitionGraph.empty
  |> TransitionSet.fold (fun t g -> TransitionGraph.add_edge_e g t) subgraph_transitions

let check_subgraphs_criteria get_timebound orig_graph entry_loc target_loc subs = 
  let all_guards_tautology =
    subs
    |> List.for_all 
         (TransitionSet.for_all (SMT.Z3Solver.unsatisfiable % Formula.neg % Formula.mk % TransitionLabel.guard % Transition.label)
           % TransitionGraph.transitions)
  in

  let termination = 
    subs
    |> List.for_all 
        (fun subgraph -> 
           TransitionGraph.transitions subgraph
           |> TransitionSet.for_all ((fun b -> not (Bound.is_infinity b|| Bound.is_minus_infinity b)) % get_timebound)
        )
  in

  let no_interleaving = 
    let helper_fun g rest = 
      ListMonad.(rest >>= (fun g2 -> 
                             let lset1 = TransitionGraph.locations g in
                             let lset2 = TransitionGraph.locations g2 in
                             LocationSet.disjoint lset1 lset2 |> pure) )
      |> List.for_all identity
    in

    let rec rec_procedure subgs =
      match subgs with
        | [] -> failwith "impossible"
        | [g] -> true
        | (g :: gs) -> helper_fun g gs && rec_procedure gs
    in
    rec_procedure subs
  in

  let only_one_gt_going_to_loc = 
    let one_gtransition_to_loc subgraph = 
      let locations = TransitionGraph.locations subgraph in
      let transitions =
        TransitionGraph.transitions subgraph
        |> TransitionSet.filter (fun (l,_,l') -> LocationSet.mem l locations && Location.equal l' target_loc)
      in
      GeneralTransitionSet.from_transitionset transitions
      |> GeneralTransitionSet.cardinal
      |> (=) 1
    in
    subs |> List.for_all (one_gtransition_to_loc)
  in

  let all_trans_of_gt_going_to_loc = 
    let for_subgraph subgraph =
      TransitionGraph.transitions subgraph
      |> GeneralTransitionSet.from_transitionset
      |> GeneralTransitionSet.filter 
           (TransitionSet.exists (fun (_,_,l') -> Location.equal l' target_loc) % GeneralTransition.transitions)
      |> GeneralTransitionSet.for_all ((=) 1 % LocationSet.cardinal % GeneralTransition.targets)
    in
    List.for_all for_subgraph subs
  in

  let only_enter_through_entry_loc = 
    let cut_graph = TransitionGraph.remove_vertex orig_graph entry_loc in
    let for_subgraph subgraph = 
      let locations_in_sub = TransitionGraph.locations subgraph in
      let locations_notin_sub = LocationSet.diff (TransitionGraph.locations orig_graph) locations_in_sub
      in
      TransitionGraph.transitions cut_graph
      |> TransitionSet.filter (fun (l,_,l') -> LocationSet.mem l  locations_in_sub &&
                                               LocationSet.mem l' locations_notin_sub)
      |> TransitionSet.cardinal
      |> (=) 0
    in
    List.for_all for_subgraph subs
  in

  let criteria = 
    [all_guards_tautology;termination;no_interleaving;only_one_gt_going_to_loc;only_enter_through_entry_loc;
     all_trans_of_gt_going_to_loc] 
  in
  List.for_all identity criteria

let graph_connected_with_orig_graph (orig_graph: TransitionGraph.t) (subgraph: TransitionGraph.t) (loc : Location.t): bool = 
  let locations = TransitionGraph.locations subgraph in
  let outgoing_trans = TransitionGraph.transitions orig_graph 
                       |> TransitionSet.filter (fun (l,_,_) -> LocationSet.mem l locations)
  in
  outgoing_trans
  |> TransitionSet.filter (fun (_,_,l) -> Location.equal l loc)
  |> (<) 0 % TransitionSet.cardinal

let rec get_pr (graph: TransitionGraph.t) (start: Location.t) (locs: LocationSet.t) (loc: Location.t) get_timebound: (Location.t->float
option) = 
  let subgraph_cardinals_combined transitions_and_graphs_list = 
    List.fold_left (fun ctr (_,g) -> ctr + (LocationSet.cardinal (TransitionGraph.locations g) )) 0 transitions_and_graphs_list
  in

  (*Search for candidates of subgraphs *)
  let candidates = 
    locs
    |> LocationSet.to_list
    |> List.filter (fun ml -> every_path_enters_loc graph start ml loc)
    |> List.map (fun l -> (l,gt_outgoing (TransitionGraph.transitions graph) l))
    |> List.filter (fun (_,gtset) -> GeneralTransitionSet.cardinal gtset = 1)
    |> List.filter (fun (_,gtset) -> GeneralTransitionSet.to_list gtset |> List.hd |> GeneralTransition.targets |>
         ((<) 1 % LocationSet.cardinal) )
    (* Build subgraphs *) 
    |> List.map (fun (l,gts_from_l) -> 
        gts_from_l
        |> GeneralTransitionSet.to_list 
        |> List.map (TransitionSet.to_list % GeneralTransition.transitions)
        (* Only one GeneralTransition here so the next call always works as intended *) 
        |> List.flatten
        |> List.map Transition.target
        |> List.fold_left (flip LocationSet.add) LocationSet.empty
        |> LocationSet.to_list
        |> List.map (fun l -> (l,build_subgraph graph l loc))
        |> List.filter (fun (_,subgraph) -> graph_connected_with_orig_graph graph subgraph loc) 
        |> fun x -> (l,x) )
    (* Sort by cardinality of all subgraphs combined *)
    |> List.sort (fun (_,a) (_,b) -> (flip Int.compare) (subgraph_cardinals_combined a) (subgraph_cardinals_combined b)) 
    |> List.filter (fun (lentry,subs) -> List.map Tuple2.second subs |> check_subgraphs_criteria get_timebound graph lentry loc)
  in 
  
  (* Construction of the pr function *)
  match candidates with
  | [] -> const None
  | ((lentry,subs) :: _) -> 
      (fun l -> 
        let is_contained_in_sub = 
          subs
          |> List.filter (LocationSet.mem l % TransitionGraph.locations % Tuple2.second)
          (* This list contains at most one element *) 
          |> Util.safe_head
        in
        match is_contained_in_sub with
        | None -> None
        | Some (sub_start,subgraph) -> 
          let rec_result = (get_pr graph sub_start (TransitionGraph.locations subgraph) l get_timebound) l in
          let rel_trans_outgoing = 
            graph
            |> TransitionGraph.transitions 
            |> GeneralTransitionSet.from_transitionset
            |> GeneralTransitionSet.filter (Location.equal lentry % GeneralTransition.start)
            |> GeneralTransitionSet.to_list
            (* There is always exactly one outgoing gt *)
            |> List.hd
            |> GeneralTransition.transitions
            |> TransitionSet.filter (fun t -> List.exists (Location.equal (Transition.target t) % Tuple2.first) subs)
          in
          let total_probability = 
            rel_trans_outgoing |> TransitionSet.to_list |> List.map (TransitionLabel.probability % Transition.label) |> List.fsum
          in
          let probability_reaching_loc l =
            rel_trans_outgoing |> TransitionSet.filter (Location.equal l % Transition.target) 
            |> TransitionSet.to_list |> List.map (TransitionLabel.probability % Transition.label)
            |> List.fsum 
          in
          match rec_result with
          | None -> Some (probability_reaching_loc sub_start /. total_probability)
          | Some p -> Some ((probability_reaching_loc sub_start /. total_probability) *. p)
      )

(** Computes a bound for a trivial scc. That is an scc which consists only of one result variable without a loop to itself.
    Corresponds to 'SizeBounds for trivial SCCs'. *)
let compute kind program get_sizebound get_timebound (gt,var,loc) =
  let execute () =
    let graph = Program.graph program in
    let start_loc = Program.start program in
    let locations = TransitionGraph.locations graph in 
    let transition_location = GeneralTransition.start gt in
    let pr_func = get_pr graph start_loc locations transition_location get_timebound in
    let (lsb: ExpLocalSizeBound.t Option.t) =
      ExpLocalSizeBound.sizebound_local_rv program kind (gt,var,loc)
    in
    if Program.is_initial_gt program gt then
      ExpLocalSizeBound.(
      lsb
      |> Option.map as_bound
      |? default kind)
    else
      ExpLocalSizeBound.(
      lsb
      (* TODO *)
      |> Option.map (fun lsb -> incoming_bound kind program get_sizebound lsb gt pr_func)
      |? default kind)
      
  in Logger.with_log logger Logger.DEBUG
                     (fun () -> "compute trivial bound", ["kind", show_kind kind;
                                                          "rv", GTRV.to_id_string (gt,var)])
                     ~result:RealBound.to_string
                     execute

