open Batteries
open Polynomials

(* Counts file and creates always a new file useful for debugging. *)
let counter = ref 0

(* To allow several parallel executions of KoAT, we create cfr-output folders with unique names. *)
let uid = ref ""

module Make (Bound : BoundType.Bound) = struct
  open ProgramModules
  module Approximation = Approximation.MakeForClassicalAnalysis (Bound) (ProgramModules)

  exception CFRefinementCRASH

  module TransitionGraphWeight (Value : PolyTypes.Ring) = struct
    type t = Value.t
    type edge = TransitionGraph.E.t

    let weight (x : edge) = Value.one
    let compare x y = 0
    let add x y = Value.add x y
    let zero = Value.zero
  end

  module DjikstraTransitionGraph = Graph.Path.Dijkstra (TransitionGraph) (TransitionGraphWeight (OurInt))

  module SCCSetCompare = struct
    type t = TransitionSet.t

    let compare t1 t2 = Base.Set.compare_direct t1 t2
  end

  module SCCSet = Set.Make (SCCSetCompare)
  module GraphPrint = GraphPrint.MakeFromClassical (ProgramModules)

  (* TODO *)
  let logger = Logging.(get CFR)

  let add_to_proof program bound =
    ProofOutput.add_to_proof_with_format
    @@ FormattedString.(
         fun format ->
           mk_header_small (mk_str "CFR: Improvement to new bound with the following program: ")
           <> mk_paragraph
                (mk_str "new bound: " <> mk_newline
                <> mk_paragraph (mk_str bound)
                <> mk_str "cfr-program: " <> mk_newline
                <> mk_paragraph (Program.to_formatted_string ~pretty:true program))
           <>
           match format with
           | Formatter.Html -> mk_raw_str GraphPrint.(print_system_pretty_html program)
           | _ -> FormattedString.Empty)


  (* SCCs that contain a non-linear transition, its not guaranteed that they are minimal *)
  let nonLinearSCCs (nonLinearTransitions : TransitionSet.t) (program : Program.t) =
    Program.sccs program
    |> Base.List.filter ~f:(fun scc ->
           nonLinearTransitions
           |> Base.Set.exists ~f:(fun nonLinearTransition -> scc |> flip Base.Set.mem nonLinearTransition))


  (* Transforms (non-minimal) TransitionSets (i.e. our SCCs) to TransitionGraphs to apply Djikstra as the next step. *)
  let getTransitionGraph (nonLinearSCCs : TransitionSet.t list) =
    Logger.log logger Logger.INFO (fun () ->
        ( "getTransitionGraph",
          [ ("input: " ^ String.concat "\n" (List.map TransitionSet.to_string nonLinearSCCs), "") ] ));
    nonLinearSCCs
    |> List.map (fun nonLinearSCC ->
           nonLinearSCC
           |> Base.Set.fold
                ~f:(fun graph transition -> transition |> TransitionGraph.add_edge_e graph)
                ~init:TransitionGraph.empty)


  (* Apply Dijkstra on ProgramGraph and get shortest l2 -> l1 path
     and add transition l1 -> l2 (thus, we get a circle, a minimal SCC) on all SCCs *)
  let applyDijkstra (nonLinearTransitions : TransitionSet.t) (program : Program.t) =
    let allNonLinearSCCs = program |> nonLinearSCCs nonLinearTransitions in
    Logger.log logger Logger.INFO (fun () ->
        ( "applyDijkstra",
          [
            ( "non-linear sccs: "
              ^ Base.List.fold
                  ~f:(fun str1 str2 -> str1 ^ "\n" ^ str2)
                  ~init:""
                  (Base.List.map ~f:TransitionSet.to_string allNonLinearSCCs),
              "" );
          ] ));

    let graphs = allNonLinearSCCs |> getTransitionGraph in
    Logger.log logger Logger.INFO (fun () ->
        ( "applyDijkstra",
          [
            ( "non-linear sccs graph: "
              ^ String.concat "\n" (List.map (TransitionSet.to_string % TransitionGraph.transitions) graphs),
              "program:" ^ Program.to_string program );
          ] ));
    nonLinearTransitions |> Base.Set.to_list
    |> List.map (fun (l1, x, l2) ->
           let path, _ =
             DjikstraTransitionGraph.shortest_path
               (graphs |> List.find (fun graph -> TransitionGraph.mem_edge graph l1 l2))
               l2 l1
           in
           [ (l1, x, l2) ] @ path)


  (* Add transitions which occur in the program and are parallel to a transition from the SCC. *)
  let parallelTransitions (program : Program.t) scc =
    TransitionSet.empty
    |> List.fold_right
         (fun transition res -> res |> Base.Set.union (transition |> Program.parallel_transitions program))
         scc


  (* Constructs minimal SCCs containing a non-linear transition and all parallel edges. *)
  let minimalSCCs (nonLinearTransitions : TransitionSet.t) (program : Program.t) =
    program |> applyDijkstra nonLinearTransitions |> List.map (fun scc -> parallelTransitions program scc)


  let rec disjoint_sccs original_sccs merged_scc =
    let disjoint, non_disjoint =
      List.partition
        (fun scc -> Base.Set.are_disjoint (TransitionSet.locations scc) (TransitionSet.locations merged_scc))
        original_sccs
    in
    if List.is_empty non_disjoint then
      merged_scc
    else
      disjoint_sccs disjoint (List.fold Base.Set.union merged_scc non_disjoint)


  (* Merges non-disjoint SCCS *)
  let minimalDisjointSCCs (original_sccs : TransitionSet.t list) =
    original_sccs |> List.map (disjoint_sccs original_sccs) |> SCCSet.of_list |> fun tmp ->
    SCCSet.filter
      (fun scc1 ->
        not
          (SCCSet.exists
             (fun scc2 -> Base.Set.is_subset scc1 ~of_:scc2 && not (Base.Set.equal scc1 scc2))
             tmp))
      tmp


  (* After parsing the program outputted by iRankFinder the arg variables will be called Arg_0, Arg_1. *)
  (* But suppose we have already preprocessed our program and eliminated argument variable Arg_0. I.e. argument variables are Arg_1,.. *)
  (* Then we have to correspondingly rename the argument variables from iRankFinder *)
  let rename_arg_vars original_program program_cfr =
    let rename_map =
      Base.Sequence.zip Var.args
        (Base.Set.to_sequence ~order:`Increasing @@ Program.input_vars original_program)
      |> RenameMap.from % Base.Sequence.to_list
    in
    Program.map_transitions (Transition.rename rename_map) program_cfr


  (* Creates a file containing irankfinder, applies irankfinder and returns the resulting program. *)
  let applyIrankFinder (scc_program : Program.t) =
    if String.equal !uid "" then (
      uid := string_of_int (Unix.getpid ()) ^ "_" ^ string_of_float (Unix.gettimeofday ());
      try Unix.mkdir ("./tmp_" ^ !uid) 0o700 with
      | Unix.Unix_error (Unix.EEXIST, _, _) -> ());
    counter := !counter + 1;
    Program.to_file scc_program ("./tmp_" ^ !uid ^ "/tmp_scc" ^ string_of_int !counter ^ ".koat");
    let tmp =
      Sys.command
        ("CFRefinement -cfr-it 1 -cfr-call -cfr-head -cfr-head-deep --no-unfold all --output-format koat \
          --output-destination ./tmp_" ^ !uid ^ "/tmp --file ./tmp_" ^ !uid ^ "/tmp_scc"
       ^ string_of_int !counter ^ ".koat > /dev/null 2>&1")
    in
    if tmp != 0 then
      raise CFRefinementCRASH;
    "./tmp_" ^ !uid ^ "/tmp/tmp_scc" ^ string_of_int !counter ^ "_cfr1.koat"
    |> Readers.read_input ~rename:false false
    |> rename_arg_vars scc_program


  let rename_matching_trans (l_original : Location.t) (l_cfr : Location.t) transitions =
    transitions
    |> TransitionSet.map ~f:(fun (l, g, l') ->
           if Location.equal l l_cfr then
             (l_original, g, l')
           else
             (l, g, l'))
    |> TransitionSet.map ~f:(fun (l, g, l') ->
           if Location.equal l' l_cfr then
             (l, g, l_original)
           else
             (l, g, l'))


  (* Finds first location which matches to the entry-location l ->  l' from the original program.
     We assume that we only get transitions starting in the initial location of cfr program*)
  let find_matching_locations (l' : Location.t) (entry_transitions_cfr : TransitionSet.t) =
    entry_transitions_cfr |> Base.Set.to_sequence
    |> Base.Sequence.filter ~f:(fun (_, _, l'_cfr) ->
           String.exists (Location.to_string l'_cfr) ("n_" ^ Location.to_string l' ^ "__"))
    |> Base.Sequence.map ~f:(fun (_, _, l') -> l')
    |> LocationSet.of_sequence


  let rename_entry_transition (entry_locations : LocationSet.t) (initial_location : Location.t)
      (transitions : TransitionSet.t) =
    entry_locations
    |> Base.Set.fold
         ~f:(fun merged_trans location ->
           let all_l' =
             find_matching_locations location
               (Base.Set.filter
                  ~f:(fun (l, _, _) ->
                    Location.equal l (Location.of_string ("n_" ^ Location.to_string initial_location)))
                  transitions)
           in
           all_l'
           |> Base.Set.fold ~f:(fun tset l' -> rename_matching_trans location l' tset) ~init:merged_trans)
         ~init:transitions


  (* ------------------------------------- *)

  (* Adds original outgoing transitions, i.e.,  program and program_cfr are path equivalent. *)
  let outgoing_transitions (outgoing_trans : Transition.t list) (scc_cfr : TransitionSet.t) =
    let locations_cfr =
      Base.Set.fold
        ~f:(fun locations (l, _, l') -> locations |> flip Base.Set.add l |> flip Base.Set.add l')
        scc_cfr ~init:LocationSet.empty
    in
    TransitionSet.empty
    |> List.fold_right
         (fun (l, t, l') set ->
           locations_cfr
           |> Base.Set.filter ~f:(fun l_cfr ->
                  String.exists (Location.to_string l_cfr) ("_" ^ Location.to_string l ^ "__"))
           |> Base.Set.to_sequence
           |> TransitionSet.of_sequence
              % Base.Sequence.map ~f:(function l_cfr -> (l_cfr, TransitionLabel.fresh_id t, l'))
           |> Base.Set.union set)
         outgoing_trans


  (* We just output guard && invariant. Hence we need to separate invariants after irankfinder call. *)
  let restore_invariants (program : Program.t) trans =
    let org_trans = Program.transitions program in
    let trans_without_entry =
      Base.Set.filter
        ~f:
          (not
          % String.equal ("n_" ^ Location.to_string (Program.start program))
          % Location.to_string % Transition.src)
        trans
    in
    let matching_trans (l, _, l') =
      Base.Set.filter
        ~f:(fun (l_org, _, l'_org) ->
          String.exists (Location.to_string l) ("n_" ^ Location.to_string l_org ^ "__")
          && String.exists (Location.to_string l') ("n_" ^ Location.to_string l'_org ^ "__"))
        org_trans
      |> Base.Set.choose_exn
    in
    TransitionSet.map
      ~f:(fun (l, t, l') ->
        let _, t_org, _ = matching_trans (l, t, l') in
        (l, TransitionLabel.separate_guard_invariant t (TransitionLabel.invariant t_org), l'))
      trans_without_entry
    |> Base.Set.union (Base.Set.diff trans trans_without_entry)


  let apply_cfr (nonLinearTransitions : TransitionSet.t) (program : Program.t) =
    let initial_location = Program.start program
    and minimalDisjointSCCs = program |> minimalSCCs nonLinearTransitions |> minimalDisjointSCCs in
    if SCCSet.is_empty minimalDisjointSCCs then
      MaybeChanged.same program
    else
      let f_iteration scc mc =
        let merged_program = MaybeChanged.unpack mc in
        (fun sccs ->
          Logger.log logger Logger.INFO (fun () ->
              ( "minimalSCC",
                [
                  ( "non-linear transitions: "
                    ^ TransitionSet.to_string (Base.Set.inter nonLinearTransitions scc),
                    "\n minimalSCC: " ^ TransitionSet.to_string scc );
                ] )))
          scc;
        let unit_cost = Base.Set.for_all ~f:(fun trans -> Polynomial.(equal (Transition.cost trans) one)) in
        (*if there are costs which are not one then we cannot apply irankfinder *)
        if (not (unit_cost scc)) || Base.Set.is_empty (Program.input_vars merged_program) then
          mc
        else
          let scc_list = Base.Set.to_list scc in
          let entry_locations =
            LocationSet.of_list
              (List.map
                 (fun (_, _, l) -> l)
                 (Program.entry_transitions_with_logger logger merged_program scc_list))
          in
          let entry_transitions =
            List.map
              (fun (l, t, l') -> (initial_location, t, l'))
              (Program.entry_transitions_with_logger logger merged_program scc_list)
          in
          try
            let program_cfr =
              Program.from_sequence initial_location (Base.Sequence.of_list @@ entry_transitions @ scc_list)
              |> applyIrankFinder
            (* Prepares transitions created by irankfinder to merge. Hier müssen noch die Variablen x' = update(x) verändert werden. *)
            and map =
              RenameMap.from_native
                (List.map
                   (fun var ->
                     ( String.replace ~sub:"_" ~by:"" ~str:(Var.to_string var) |> Tuple2.second,
                       Var.to_string var ))
                   (Program.input_vars merged_program |> Base.Set.to_list))
            in
            let transitions_cfr =
              program_cfr |> Program.transitions |> restore_invariants merged_program
              |> rename_entry_transition entry_locations initial_location
              |> Base.Set.filter ~f:(fun (l, _, _) ->
                     not (BatString.equal ("n_" ^ Location.to_string initial_location) (Location.to_string l)))
              |> TransitionSet.map ~f:(fun t -> Transition.rename map t)
            in

            let removable_loc =
              let locations =
                List.fold_right
                  (fun (l, _, l') locations -> Base.Set.add locations l |> flip Base.Set.add l')
                  scc_list LocationSet.empty
              in
              Base.Set.filter
                ~f:(fun l ->
                  Base.Set.exists
                    ~f:(fun (_, _, l') -> Location.equal l l')
                    (Base.Set.diff (Program.transitions merged_program) scc))
                locations
              |> Base.Set.diff locations
            in

            (* Merges irankfinder and original program. *)
            let processed_program =
              merged_program |> Program.transitions |> Base.Set.union transitions_cfr
              |> Base.Set.union
                   (outgoing_transitions
                      (Program.outgoing_transitions logger merged_program scc_list)
                      transitions_cfr)
              |> flip Base.Set.diff scc
              |> Base.Set.filter ~f:(fun (l, _, _) -> not (Base.Set.mem removable_loc l))
              |> Base.Set.to_list
              |> Program.from_sequence initial_location % Base.Sequence.of_list
            in
            MaybeChanged.changed processed_program
          with
          | CFRefinementCRASH -> mc
      in
      MaybeChanged.same program |> SCCSet.fold f_iteration minimalDisjointSCCs
end
