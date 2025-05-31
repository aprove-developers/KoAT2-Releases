open Batteries
open Formulas
open Constraints
open Atoms
open Polynomials
module Solver = SMT.IncrementalZ3Solver
module Valuation = Valuation.Make (OurInt)

type measure = [ `Cost | `Time ] [@@deriving show]
type constraint_type = [ `Non_Increasing | `Decreasing ]

let logger = Logging.(get RRF)

module Make (Bound : BoundType.Bound) (PM : ProgramTypes.ClassicalProgramModules) = struct
  open PM

  type rrf = (Location.t -> Polynomial.t) * (Location.t -> Polynomial.t) * (Location.t -> Polynomial.t)
  type t = { rank : rrf; decreasing : Transition.t; non_increasing : TransitionSet.t }

  module TemplateTable = Hashtbl.Make (Location)

  module CoeffsTable = Hashtbl.Make (struct
    type t = Location.t * Var.t

    let equal (l1, v1) (l2, v2) = Location.equal l1 l2 && Var.equal v1 v2
    let hash = Hashtbl.hash
  end)

  type rrf_problem = {
    program : PM.Program.t;
    measure : measure;
    make_non_increasing : Transition.t Array.t;
    make_decreasing : Transition.t;
    unbounded_vars : Transition.t -> VarSet.t;
    is_time_bounded : Transition.t -> bool;
  }

  type ranking_cache = {
    rank_func : t option ref;
    template_table :
      ParameterPolynomial.t TemplateTable.t (* r_tf *)
      * ParameterPolynomial.t TemplateTable.t (* r_t *)
      * ParameterPolynomial.t TemplateTable.t (* r_f *);
    coeffs_table : VarSet.t CoeffsTable.t;
    constraint_cache_transition : (constraint_type * int, Formula.t) Hashtbl.t;
    constraint_cache_varrec : (constraint_type * int * int, Formula.t) Hashtbl.t;
  }

  let new_cache () =
    let new_template_table () = (TemplateTable.create 10, TemplateTable.create 10, TemplateTable.create 10) in
    {
      rank_func = ref None;
      template_table = new_template_table ();
      coeffs_table = CoeffsTable.create 10;
      constraint_cache_transition = Hashtbl.create 10;
      constraint_cache_varrec = Hashtbl.create 10;
    }


  (* Cache does not depend on measure since the cache is unique for each measure *)
  let constraint_cache_transition cache =
    Util.memoize cache.constraint_cache_transition ~extractor:(fun (_, constraint_type, t) ->
        (constraint_type, Transition.id t))


  let constraint_cache_varrec cache =
    Util.memoize cache.constraint_cache_varrec ~extractor:(fun (_, constraint_type, v, t) ->
        (constraint_type, VarRec.hash v, Transition.id t))


  let decreaser measure cost =
    match measure with
    | `Cost -> cost
    | `Time -> Polynomial.one


  (* method transforms polynome to parapolynom*)
  let as_parapoly update var =
    match Base.Map.find update var with
    (* Correct? In the nondeterministic case we just make it deterministic? *)
    | None -> ParameterPolynomial.of_var var
    | Some p -> ParameterPolynomial.of_polynomial p


  (** Given a list of variables an affine template-polynomial is generated*)
  let ranking_template cache location (vars : VarSet.t) : ParameterPolynomial.t * Var.t list =
    let vars = Base.Set.elements vars in
    let num_vars = List.length vars in
    let fresh_vars = Var.fresh_id_list Var.Int num_vars in
    let fresh_coeffs = List.map Polynomial.of_var fresh_vars in

    (if Option.is_some cache && Option.is_some location then
       (* store fresh_vars *)
       let cache = Option.get cache and location = Option.get location in
       let coeff_table = cache.coeffs_table in
       List.iter
         (* (fun (v,v') -> CoeffTable.add coeff_table (location,v) v') *)
         (fun (v, v') ->
           CoeffsTable.modify_def VarSet.empty (location, v)
             (Base.Set.union (VarSet.singleton v'))
             coeff_table)
         (List.combine vars fresh_vars));

    let linear_poly = ParameterPolynomial.of_coeff_list fresh_coeffs vars in
    let constant_var = Var.fresh_id Var.Int () in
    let constant_poly = ParameterPolynomial.of_constant (Polynomial.of_var constant_var) in
    (ParameterPolynomial.(linear_poly + constant_poly), List.append fresh_vars [ constant_var ])


  let rank f = f.rank
  let decreasing f = f.decreasing
  let non_increasing f = f.non_increasing

  (* output methods *)
  let rank_to_string (locations : Location.t list) (content_to_string : rrf * Location.t -> string)
      (rank : rrf) =
    locations |> List.enum
    |> Util.enum_to_string (fun l -> Location.to_string l ^ ": " ^ content_to_string (rank, l))


  let element_to_string ?(pretty = false) ((rank : rrf), (l : Location.t)) =
    let print r =
      if pretty then
        Polynomial.to_string_pretty (r l)
      else
        Polynomial.to_string (r l)
    in

    let x, y, z = Tuple3.mapn print rank in
    "(" ^ x ^ "," ^ y ^ "," ^ z ^ ")"


  let only_rank_to_string { rank; decreasing; non_increasing } =
    let locations = non_increasing |> TransitionSet.locations |> Base.Set.to_list in
    rank_to_string locations element_to_string rank


  let to_string { rank; decreasing; non_increasing } =
    "{rec. rank:"
    ^ only_rank_to_string { rank; decreasing; non_increasing }
    ^ "; decreasing:" ^ Transition.to_id_string decreasing ^ "; non-increasing:"
    ^ TransitionSet.to_id_string non_increasing
    ^ "}"


  let compute_proof { rank; decreasing; non_increasing } bound program format =
    let module GraphPrint = GraphPrint.MakeForClassicalAnalysis (PM) in
    let color_map =
      Base.Set.fold
        ~f:(fun colourmap t -> OurBase.Map.set ~key:t ~data:GraphPrint.Blue colourmap)
        non_increasing ~init:GraphPrint.empty_color_map
      |> OurBase.Map.set ~key:decreasing ~data:GraphPrint.Red
    in
    let locations = non_increasing |> TransitionSet.locations |> Base.Set.to_list in
    FormattedString.(
      mk_header_small (mk_str ("RRF for transition " ^ Transition.to_string_pretty decreasing ^ ":"))
      <> mk_paragraph
           ((match bound with
            | Some b ->
                mk_str "new bound:" <> mk_newline <> mk_paragraph (mk_str (Bound.to_string ~pretty:true b))
            | _ -> FormattedString.Empty)
           <> mk_str "RRF:" <> mk_newline
           <> (locations
              |> List.map (fun l ->
                     "• " ^ Location.to_string l ^ ": " ^ element_to_string ~pretty:true (rank, l))
              |> List.map mk_str_line |> mappend |> mk_paragraph))
      <>
      match format with
      | Formatter.Html -> FormattedString.mk_raw_str (GraphPrint.print_system_pretty_html ~color_map program)
      | _ -> FormattedString.Empty)


  let add_to_proof rrf bound program = ProofOutput.add_to_proof_with_format (compute_proof rrf bound program)

  module UnliftedBound = UnliftedBounds.UnliftedTimeBound.Make (PM) (Bound)

  let to_unlifted_bound program t =
    let evaluated_rank_for_entry_loc entry_loc =
      let rtf, rt, rf = Tuple3.mapn (fun r -> Bound.of_intpoly @@ r entry_loc) t.rank
      and nfc =
        Bound.of_int
        @@ List.max
             (List.map (OurBase.Set.length % Transition.rec_vars) @@ OurBase.Set.to_list t.non_increasing)
      in
      Bound.(rt + (rf * (one + (rt * (one + nfc))) * exp (nfc * rtf) rf))
    in
    let locs = OurBase.Set.map (module Location) t.non_increasing ~f:Tuple3.first in
    UnliftedBound.mk_from_program_fcs logger ~handled_transitions:t.non_increasing
      ~measure_decr_transitions:(TransitionSet.singleton t.decreasing)
      ~compute_proof:
        (Option.some @@ fun ~get_timebound ~get_sizebound _ bound -> compute_proof t (Some bound) program)
      program
      (fun (_, _, l') -> evaluated_rank_for_entry_loc l')
      (fun t' ->
        ( OurBase.Set.fold
            ~f:(fun b v -> Bound.add (evaluated_rank_for_entry_loc (VarRec.return_loc v)) b)
            ~init:Bound.zero (Transition.rec_vars t'),
          OurBase.Set.filter ~f:(fun v -> OurBase.Set.mem locs (VarRec.return_loc v)) (Transition.rec_vars t')
        ))


  let compute_ranking_templates_ (vars : VarSet.t) (locations : Location.t list) ranking_template_
      (tt1, tt2, tt3) to_string : unit =
    let execute template_table =
      let ins_loc_prf location =
        (* Each location needs its own ranking template with different fresh variables *)
        let parameter_poly, fresh_vars = ranking_template_ location vars in
        (location, parameter_poly, fresh_vars)
      in
      let templates = List.map ins_loc_prf locations in
      templates
      |> List.iter (fun (location, polynomial, _) -> TemplateTable.add template_table location polynomial)
    in
    Logger.with_log logger Logger.DEBUG
      (fun () -> ("compute_rrf_templates", []))
      ~result:(fun () ->
        let to_string t =
          TemplateTable.enum t
          |> Util.enum_to_string (fun (location, polynomial) ->
                 Location.to_string location ^ ": " ^ to_string polynomial)
        in
        to_string tt1 ^ "\n" ^ to_string tt2 ^ "\n" ^ to_string tt3)
      (fun () ->
        execute tt1;
        execute tt2;
        execute tt3)


  let compute_ranking_templates cache (vars : VarSet.t) (locations : Location.t list) : unit =
    compute_ranking_templates_ vars locations
      (ranking_template (Option.some cache) % Option.some)
      cache.template_table ParameterPolynomial.to_string


  let apply_farkas pre concl =
    ParameterConstraint.(farkas_transform (of_constraint @@ Constraint.drop_nonlinear pre)) concl


  (* Methods define properties of rrf *)

  (* Method for RRF and transitions*)
  let constraint_ (measure, constraint_type) (update, guard, cost) template_l template_l' : Formula.t =
    let atom =
      match constraint_type with
      | `Decreasing ->
          ParameterAtom.Infix.(
            template_l
            >= ParameterPolynomial.(
                 ParameterPolynomial.of_polynomial (decreaser measure cost)
                 + substitute_f (as_parapoly update) template_l'))
      | `Non_Increasing ->
          ParameterAtom.Infix.(
            template_l >= ParameterPolynomial.substitute_f (as_parapoly update) template_l')
    in
    apply_farkas guard atom |> Formula.mk


  let bounded_ (measure, constraint_type) (update, guard, cost) template_l : Formula.t =
    match constraint_type with
    | `Decreasing ->
        let atom = ParameterAtom.Infix.(template_l > ParameterPolynomial.zero) in
        apply_farkas guard atom |> Formula.mk
    | `Non_Increasing -> Formula.mk_true


  let transition_constraint (template_table, measure, constraint_type, (l, t, l')) : Formula.t =
    let template_l = TemplateTable.find template_table l
    and template_l' = TemplateTable.find template_table l' in
    Formula.mk_and
      (constraint_ (measure, constraint_type)
         TransitionLabel.TransitionLabelNonRec.(update_map t, guard t, cost t)
         template_l template_l')
      (bounded_ (measure, constraint_type)
         TransitionLabel.TransitionLabelNonRec.(update_map t, guard t, cost t)
         template_l)


  let transition_constraint_ cache (measure, constraint_type, (l, (t : TransitionLabel.t), l')) : Formula.t =
    let t_non_rec = TransitionLabel.overapprox_rec_updates t in
    match constraint_type with
    | `Decreasing ->
        if TransitionLabel.has_rec_calls t then
          transition_constraint (Tuple3.first cache.template_table, measure, `Decreasing, (l, t_non_rec, l'))
        else
          transition_constraint (Tuple3.second cache.template_table, measure, `Decreasing, (l, t_non_rec, l'))
    | `Non_Increasing ->
        let f1, f2, f3 =
          Tuple3.mapn
            (fun template_table ->
              transition_constraint (template_table, measure, `Decreasing, (l, t_non_rec, l')))
            cache.template_table
        in
        Formula.(mk_and f1 (mk_and f2 f3))


  let transition_constraint cache = constraint_cache_transition cache (transition_constraint_ cache)

  let decreasing_transition_constraint cache measure transition =
    Formula.mk_and
      (transition_constraint cache (measure, `Decreasing, transition))
      (transition_constraint cache (measure, `Non_Increasing, transition))


  let non_increasing_transition_constraint cache measure transition =
    if Transition.has_rec_calls transition then
      (* If a transition has rec. calls it must always be decreasing. *)
      decreasing_transition_constraint cache measure transition
    else
      transition_constraint cache (measure, `Non_Increasing, transition)


  let varrec_constraint (template_table, measure, constraint_type, (l, t, _), varrec) : Formula.t =
    let template_l = TemplateTable.find template_table l
    and template_l' = TemplateTable.find template_table (VarRec.return_loc varrec) in
    Formula.mk_and
      (constraint_ (measure, constraint_type)
         TransitionLabel.(VarRec.update varrec, guard t, cost t)
         template_l template_l')
      (bounded_ (measure, constraint_type) TransitionLabel.(VarRec.update varrec, guard t, cost t) template_l)


  let varrec_constraint_ cache (measure, constraint_type, varrec, t) : Formula.t =
    match constraint_type with
    | `Decreasing -> varrec_constraint (Tuple3.third cache.template_table, measure, `Decreasing, t, varrec)
    | `Non_Increasing ->
        let f1, f2, f3 =
          Tuple3.mapn
            (fun template_table -> varrec_constraint (template_table, measure, `Non_Increasing, t, varrec))
            cache.template_table
        in
        Formula.(mk_and f1 (mk_and f2 f3))


  let varrec_constraint cache = constraint_cache_varrec cache (varrec_constraint_ cache)

  let varrec_constraint cache measure varrec transition =
    Formula.mk_and
      (varrec_constraint cache (measure, `Decreasing, varrec, transition))
      (varrec_constraint cache (measure, `Non_Increasing, varrec, transition))


  (** A valuation is a function which maps from a finite set of variables to values *)
  let rank_from_valuation_ valuation =
    ParameterPolynomial.eval_coefficients (fun var -> Valuation.eval_opt var valuation |? OurInt.zero)


  let rank_from_valuation cache valuation location =
    rank_from_valuation_ valuation (TemplateTable.find cache location)


  let make cache decreasing_transition non_increasing_transitions valuation =
    {
      rank = Tuple3.mapn (fun cache -> rank_from_valuation cache valuation) cache.template_table;
      decreasing = decreasing_transition;
      non_increasing = non_increasing_transitions;
    }


  let entry_transitions_from_non_increasing program non_increasing =
    let all_possible_pre_trans =
      Stack.enum non_increasing
      |> Enum.fold (fun tset -> Base.Set.union tset % Program.pre program) TransitionSet.empty
    in
    Base.Set.diff all_possible_pre_trans (TransitionSet.of_list @@ List.of_enum @@ Stack.enum non_increasing)


  let add_decreasing_constraint cache problem solver_int =
    let t = problem.make_decreasing in
    Solver.add solver_int (decreasing_transition_constraint cache problem.measure t);
    OurBase.Set.iter (Transition.rec_vars t) ~f:(fun v ->
        Solver.add solver_int (varrec_constraint cache problem.measure v t))


  let add_non_increasing_constraint cache problem solver_int transition =
    Solver.add solver_int (non_increasing_transition_constraint cache problem.measure transition);
    OurBase.Set.iter (Transition.rec_vars transition) ~f:(fun v ->
        Solver.add solver_int (varrec_constraint cache problem.measure v transition))


  let finalise_rrf cache solver_int non_increasing entry_transitions problem =
    (* Set the coefficients for all variables for which a corresponding size bound does not exist for the entry transitions to
     * 0. *)
    let entry_trans_grouped_by_loc =
      List.sort (fun (_, _, l'1) (_, _, l'2) -> Location.compare l'1 l'2) (Base.Set.to_list entry_transitions)
      |> List.group_consecutive (fun (_, _, l'1) (_, _, l'2) -> Location.equal l'1 l'2)
    in
    let unbounded_vars_at_entry_locs coeff_table =
      List.map
        (fun ts ->
          let entryloc = Transition.target (List.hd ts) in
          List.enum ts |> Enum.map problem.unbounded_vars
          |> Enum.fold Base.Set.union VarSet.empty
          |> Base.Set.to_sequence
          |> Base.Sequence.map ~f:(fun v ->
                 Base.Set.to_sequence @@ CoeffsTable.find coeff_table (entryloc, v))
          |> Base.Sequence.join |> VarSet.of_sequence)
        entry_trans_grouped_by_loc
      |> List.fold_left Base.Set.union VarSet.empty
    in

    Solver.push solver_int;
    Base.Set.iter
      ~f:(Solver.add solver_int % Formula.mk_eq Polynomial.zero % Polynomial.of_var)
      (unbounded_vars_at_entry_locs cache.coeffs_table);
    if Solver.satisfiable solver_int then (
      (* Solver.minimize_absolute solver_int !fresh_coeffs; *)
      Solver.model solver_int
      |> Option.map
           (make cache problem.make_decreasing
              (non_increasing |> Stack.enum |> List.of_enum |> TransitionSet.of_list))
      |> Option.may (fun ranking_function ->
             cache.rank_func := Some ranking_function;
             Logger.(
               log logger INFO (fun () ->
                   ( "add_rrf",
                     [
                       ("measure", show_measure problem.measure);
                       ("decreasing", Transition.to_id_string problem.make_decreasing);
                       ( "non_increasing",
                         Util.enum_to_string Transition.to_id_string (Stack.enum non_increasing) );
                       ("rank", only_rank_to_string ranking_function);
                     ] ))));
      raise Exit)
    else
      Solver.pop solver_int


  let rec backtrack cache (steps_left : int) (index : int) (solver_int : Solver.t)
      (non_increasing : Transition.t Stack.t) problem =
    let finalise_if_entrytime_bounded non_increasing =
      let entry_trans = entry_transitions_from_non_increasing problem.program non_increasing in
      if Base.Set.for_all ~f:problem.is_time_bounded entry_trans then
        finalise_rrf cache solver_int non_increasing entry_trans problem
    in

    if Solver.satisfiable solver_int then
      if steps_left == 0 then
        finalise_if_entrytime_bounded non_increasing
      else (
        for i = index to Array.length problem.make_non_increasing - 1 do
          let transition = Array.get problem.make_non_increasing i in

          Solver.push solver_int;

          add_non_increasing_constraint cache problem solver_int transition;

          Stack.push transition non_increasing;
          backtrack cache (steps_left - 1) (i + 1) solver_int non_increasing problem;
          ignore (Stack.pop non_increasing);

          Solver.pop solver_int
        done;
        finalise_if_entrytime_bounded non_increasing)


  let get_minimum_applicable_non_inc_set rrf_problem =
    let possible_non_inc_set = Base.Set.of_array (module Transition) rrf_problem.make_non_increasing in
    let rec helper min_applicable =
      (* get time_unbounded pre transitions *)
      Base.Set.to_sequence min_applicable
      |> Base.Sequence.map ~f:(PM.Program.pre rrf_problem.program)
      (* necessary since min_applicable can contain all possible pre transitions which may be outside of the current scc*)
      |> Base.Sequence.map ~f:Base.Set.to_sequence
      |> Base.Sequence.join
      |> Base.Sequence.filter ~f:(not % rrf_problem.is_time_bounded)
      |> TransitionSet.of_sequence
      (* add previously found transitions*)
      |> Base.Set.union min_applicable
      (* we can only consider scc transitions *)
      |> Base.Set.inter possible_non_inc_set
      |> fun tset ->
      if Base.Set.length tset > Base.Set.length min_applicable then
        helper tset
      else
        tset
    in
    helper (TransitionSet.singleton rrf_problem.make_decreasing)


  let compute_scc cache program rrf_problem =
    let locations = Base.Set.to_list @@ PM.TransitionGraph.locations (PM.Program.graph program) in
    let vars = PM.Program.input_vars program in
    compute_ranking_templates cache vars locations;

    let solver_int = Solver.create () in

    (* make transition decreasing*)
    add_decreasing_constraint cache rrf_problem solver_int;

    (* initial constraint propagation for incoming timebounds*)
    let min_applicable = get_minimum_applicable_non_inc_set rrf_problem in

    Logger.log logger Logger.DEBUG (fun () ->
        ( "compute_scc",
          [
            ("decreasing", Transition.to_id_string rrf_problem.make_decreasing);
            ("min_applicable_non_inc_set", TransitionSet.to_id_string min_applicable);
          ] ));
    let non_inc = Stack.of_enum @@ List.enum (Base.Set.to_list min_applicable) in
    let make_non_increasing =
      Base.Set.to_array
      @@ (Base.Set.diff (Base.Set.of_array (module Transition) rrf_problem.make_non_increasing) min_applicable
         |> Base.Set.filter ~f:(fun (l, _, l') -> not @@ Location.equal l l'))
    in
    Base.Set.iter ~f:(add_non_increasing_constraint cache rrf_problem solver_int) min_applicable;
    (try
       backtrack cache (Array.length make_non_increasing) 0 solver_int non_inc
         { rrf_problem with make_non_increasing }
     with
    | Exit -> ());

    if Option.is_none !(cache.rank_func) then
      Logger.(
        log logger WARN (fun () ->
            ( "no_rrf",
              [
                ("measure", show_measure rrf_problem.measure);
                ("transition", Transition.to_id_string rrf_problem.make_decreasing);
              ] )))


  let find_scc measure program is_time_bounded unbounded_vars scc make_decreasing =
    let cache = new_cache () in
    let locs = LocationSet.map ~f:Transition.src (OurBase.Set.add scc make_decreasing) in
    let transitions_with_looping_fc =
      OurBase.Set.filter
        ~f:(fun t ->
          OurBase.Set.exists
               ~f:(fun v -> OurBase.Set.mem locs (VarRec.return_loc v))
               (Transition.rec_vars t))
        scc
    in
    if OurBase.Set.is_empty scc || (not @@ OurBase.Set.mem transitions_with_looping_fc make_decreasing) then
      None
    else
      let rrf_problem =
        {
          program;
          measure;
          make_non_increasing = Base.Set.to_array scc;
          make_decreasing;
          unbounded_vars;
          is_time_bounded;
        }
      in
      let execute () =
        compute_scc cache program rrf_problem;
        !(cache.rank_func)
      in
      Logger.with_log logger Logger.DEBUG
        (fun () ->
          ("find_scc", [ ("measure", show_measure measure); ("scc", TransitionSet.to_id_string scc) ]))
        ~result:(Util.enum_to_string to_string % Option.enum)
        execute


  let find measure program =
    let execute () =
      Base.Sequence.of_list (PM.Program.sccs program)
      |> Base.Sequence.map ~f:(fun scc ->
             Base.Set.to_sequence scc
             |> Base.Sequence.map ~f:(find_scc measure program (const false) (const VarSet.empty) scc))
      |> Base.Sequence.join |> Base.Sequence.filter_opt
    in
    Logger.with_log logger Logger.DEBUG
      (fun () -> ("find", [ ("measure", show_measure measure) ]))
      ~result:(Util.sequence_to_string ~f:to_string)
      execute
end

include Make (Bounds.Bound) (ProgramModules)
