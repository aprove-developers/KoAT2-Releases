open Batteries
open Polynomials
open ProgramTypes
open Formulas
open Atoms
open BoundsInst
open Constraints
open PolyExponential

(* PROOF *)
let logger = Logging.(get Twn)

let proof = ref FormattedString.Empty

let proof_append f_str = proof := FormattedString.(!proof <> f_str)

let add_to_proof_graph program cycle entries =
  let color_map =
  List.fold_right (fun t -> GraphPrint.TransitionMap.add t GraphPrint.Blue) cycle GraphPrint.TransitionMap.empty
  |> List.fold_right (fun t -> GraphPrint.TransitionMap.add t GraphPrint.Red) entries in
    proof_append FormattedString.(mk_paragraph (
      match ProofOutput.get_format () with
        | Html -> FormattedString.mk_raw_str (GraphPrint.print_system_pretty_html color_map program)
        | _    -> FormattedString.Empty));
  proof_append (FormattedString.mk_str_line ("  cycle: " ^ (Util.enum_to_string Transition.to_id_string_pretty (List.enum cycle))))

(* TOPOLOGICAL ORDERING: *)

(* https://stackoverflow.com/questions/4653914/topological-sort-in-ocaml *)
exception CycleFound of int list

let dfs graph visited start_node =
  let rec explore path visited node =
    if List.mem node path    then raise (CycleFound path) else
    if List.mem node visited then visited else
      let new_path = node :: path in
      let edges    = List.assoc node graph in
      let visited  = List.fold_left (explore new_path) visited edges in
      node :: visited
  in explore [] visited start_node

let toposort graph =
  List.fold_left (fun visited (node,_) -> dfs graph visited node) [] graph

let check_triangular (t: TWNLoop.t) =
  let vars = VarSet.to_list (TWNLoop.input_vars t) in
  let n = List.length vars in
  let vars_i = List.combine vars (List.range 0 `To (n - 1)) in
  let graph = List.mapi (fun i var ->
    let vars_update =
        TWNLoop.update t var
        |? Polynomial.zero
        |> Polynomial.vars
        |> VarSet.remove var
        |> VarSet.to_list
        |> List.map (fun var -> List.assoc var vars_i) in (i, vars_update)) vars in
  let order = try toposort graph with CycleFound _ -> [] in
  List.map (fun i -> List.assoc i (List.map Tuple2.swap vars_i)) order
  |> List.rev


(* MONOTONICITY *)

let check_weakly_monotonicity (t: TWNLoop.t) =
  VarSet.for_all (fun var -> let update = TWNLoop.update t var in
                          if Option.is_none update then
                            true
                          else
                            update |> Option.get |> Polynomial.var_only_linear var) (TWNLoop.input_vars t)


(* NEGATIVITY *)

let check_weakly_negativitiy (t: TWNLoop.t) =
  VarSet.exists (fun var -> let update = TWNLoop.update t var in
                          if Option.is_none update then
                            false
                          else
                            update |> Option.get |> Polynomial.coeff_of_var var |> OurInt.is_negative) (TWNLoop.input_vars t)


let chain (t: TWNLoop.t) = TWNLoop.append t t

(* TERMINATION: *)

module SMTSolver = SMT.Z3Solver

exception Non_Terminating of (Transition.t list * Transition.t list)

let red_lt poly_list =
    let rec constraint_eq_zero i = function
    | [] -> Constraint.mk_true
    | x::xs when i == 0 -> Constraint.mk_true
    | x::xs -> Constraint.(mk_and (mk_eq x Polynomial.zero) (constraint_eq_zero (i - 1) xs))  in
    let rec formula i = function
    | [] -> Formula.mk_false
    | x::xs -> Formula.(mk_or (mk_and (mk_lt x Polynomial.zero) (constraint_eq_zero (i - 1) poly_list |> Formula.mk)) (formula (i + 1) xs))  in
    formula 1 poly_list

let red_le poly_list =
    let rec constr = function
    | [] -> Constraint.mk_true
    | x::xs -> Constraint.(mk_and (mk_eq x Polynomial.zero) (constr xs))  in
    Formula.(mk_or (red_lt poly_list) (poly_list |> constr |> mk))


module Valuation = Valuation.Make(OurInt)

let termination_ twn order npe varmap =
  let update = fun v -> match TWNLoop.update twn v with
    | Some p -> p
    | None -> Polynomial.of_var v in
  let self_impl, rest = TWNLoop.invariant twn |> List.partition (fun a -> let f = Constraint.lift a |> Formula.mk in SMTSolver.tautology (Formula.implies f (Formula.map_polynomial (Polynomial.substitute_f update) f))) in
  let formula =
    Formula.any (
    List.map (fun constr ->
      List.fold_right (fun atom formula ->
          let poly = Atom.poly atom in
          let sub_poly = PE.substitute varmap poly |> PE.remove_frac |> List.map (RationalPolynomial.normalize % Tuple4.second) in
          let formula_poly = if Atom.is_lt atom
              then sub_poly |> red_lt
              else sub_poly |> red_le in Formula.mk_and formula formula_poly |> Formula.simplify)
              (constr |> List.unique ~eq:Atom.equal) (Formula.mk_true)) ((TWNLoop.guard_without_inv twn |> Formula.mk_and (Formula.mk rest) |> Formula.constraints))) in
  let model = SMTSolver.get_model (Formula.mk_and (Formula.mk self_impl) formula |> Formula.simplify) in
  (Option.is_none model)
  |> tap (fun bool -> Logger.log logger Logger.INFO (fun () -> "termination", ["is_satisfiable", Bool.to_string (not bool)]);
                      Logger.log logger Logger.DEBUG (fun () -> "termination", ["formula", Formula.to_string formula]);
                      if Option.is_some model then Logger.log logger Logger.DEBUG (fun () -> "termination", ["model", (Option.get model |> Valuation.to_string)]);
    proof_append
        FormattedString.(
        mk_str_line ("Termination: " ^ (string_of_bool bool))
        <> mk_str_line "Formula: "
        <> (Formula.to_string_formatted formula |> mk_block)
        |> mk_paragraph);)

(* For Testing *)
let termination t =
  let twn = TWNLoop.mk_transition t in
  let order = check_triangular twn in
  let pe = PE.compute_closed_form (List.map (fun var ->
      let update_var = TWNLoop.update twn var in
      (var, if Option.is_some update_var then Option.get update_var else Polynomial.of_var var)) order) in
  let npe = PE.normalize pe in
  let varmap = Hashtbl.of_list (List.combine order npe) in
  termination_ twn order npe varmap

(* COMPLEXITY: *)

(* Computes the monotoncitiy threshold for (b1,a1) > (b2,a2), i.e., smallest m s.t for all n >= m: n^a1 * b1^n > k * n^a1 * b1^n *)
let monotonicity_th k (b1,a1) (b2,a2) =
  if OurInt.is_negative k || OurInt.is_zero k then OurInt.one
  else
    let rec test_m m =
      let tmp1 = OurInt.(mul (pow_ourint m a1) (pow_ourint b1 m)) in
      let tmp2 = OurInt.(mul (mul (pow_ourint m a2) (pow_ourint b2 m)) k) in
      if OurInt.is_ge a1 a2 then
        if (OurInt.is_gt tmp1 tmp2) then m else test_m OurInt.(add m one)
      else
        let tmp = OurRational.(mul (pow_ourint (reduce (m , OurInt.(add m one))) (OurInt.sub a2 a1)) (reduce (b1, b2))) in
        if (OurRational.(is_ge tmp one) && (OurInt.is_gt tmp1 tmp2)) then m else test_m OurInt.(add m one) in
    let rec decrease_m m =
      if OurInt.is_zero m then m
      else
        let m_dec = OurInt.(sub m one) in
        let tmp1 = OurInt.(mul (pow_ourint m_dec a1) (pow_ourint b1 m_dec)) in
        let tmp2 = OurInt.(mul (mul (pow_ourint m_dec a2) (pow_ourint b2 m_dec)) k) in
        if OurInt.is_ge tmp2 tmp1 then m else decrease_m m_dec in
    OurInt.zero |> test_m |> decrease_m

let monotonicity_th_int k (b1,a1) (b2,a2) = monotonicity_th (OurInt.of_int k) (OurInt.of_int b1, OurInt.of_int a1) (OurInt.of_int b2, OurInt.of_int a2)

let compute_kmax sub_poly = sub_poly |> List.map (OurInt.max_list % (List.map OurInt.abs) % Polynomial.coeffs) |> OurInt.max_list

let compute_N = function
  | [] -> OurInt.zero
  | x::[] -> OurInt.zero
  | x::y::[] -> OurInt.one
  | x1::x2::x3::xs ->
    let mt = List.map (fun x -> monotonicity_th OurInt.one x3 x) xs |> OurInt.max_list in
    let mt_ = monotonicity_th (OurInt.of_int ((List.length xs) + 1)) x2 x3 in
    OurInt.max mt mt_

let compute_M = function
  | [] -> OurInt.zero
  | x::[] -> OurInt.zero
  | (b1,a1)::(b2,a2)::xs when ((OurInt.is_gt b1 b2) || ((OurInt.equal b1 b2) && (OurInt.is_gt a1 OurInt.(add a2 one))))
                         -> monotonicity_th OurInt.one (b1,a1) (b2, OurInt.(add a2 one))
  | _ -> OurInt.zero


module ScaledMonomial = ScaledMonomials.Make(OurInt)
module Monomial = Monomials.Make(OurInt)
let compute_alpha_abs = function
  | [] -> Polynomial.zero
  | x::[] -> Polynomial.fold ~const:(Polynomial.of_constant % OurInt.abs) ~var:(Polynomial.of_var) ~neg:identity ~plus:Polynomial.add ~times:Polynomial.mul ~pow:Polynomial.pow x
  | xs -> List.flatten (List.map Polynomial.scaled_monomials xs)
          |> List.group (fun x y -> Monomial.compare (ScaledMonomial.monomial x) (ScaledMonomial.monomial y))
          |> List.map (fun ys -> let max = OurInt.max_list (List.map (OurInt.abs % ScaledMonomial.coeff) ys) in
                      List.first ys |> ScaledMonomial.monomial |> ScaledMonomial.make max)
          |> Polynomial.of_scaled

let compute_f atom = function
  | [] -> Bound.zero
  | x::[] -> Bound.one
  | xs ->
    let alphas = List.map (Tuple4.second) xs |> List.tl in
    Logger.log logger Logger.INFO (fun () -> "complexity.compute_f", ["alphas", (alphas |> List.enum |> Util.enum_to_string Polynomial.to_string)]);
    let alphas_abs = compute_alpha_abs alphas in
    Logger.log logger Logger.INFO (fun () -> "complexity.compute_f", ["alphas_abs", Polynomial.to_string alphas_abs]);
    let base_exp ys = List.map (fun (_,_,d,b) -> (b,d)) ys in
    let prefix ys = List.fold_left (fun acc itt -> ((List.hd acc)@[itt])::acc) [[]] ys in
    let n_ = OurInt.max_list (List.map (fun ys -> compute_N (base_exp ys)) (prefix xs)) in
    Logger.log logger Logger.INFO (fun () -> "complexity.compute_f", ["N", (OurInt.to_string n_)]);
    let m_ = OurInt.max_list (List.map (fun ys -> compute_M (base_exp ys)) (prefix xs)) in
    Logger.log logger Logger.INFO (fun () -> "complexity.compute_f", ["M", (OurInt.to_string m_)]);
    Bound.(of_poly (Polynomial.add alphas_abs alphas_abs) |> add (of_constant (OurInt.max m_ n_)) |> add (of_constant OurInt.one))
    |> tap (fun b -> Logger.log logger Logger.INFO (fun () -> "complexity.compute_f", ["2*alpha_abs+max(N,M)", Bound.to_string b]);
      proof_append (
      [ "alphas_abs: " ^ (Polynomial.to_string_pretty alphas_abs);
        "M: " ^ (OurInt.to_string m_);
        "N: " ^ (OurInt.to_string n_);
        "Bound: " ^ (Bound.to_string ~pretty:true b);
      ] |> List.map (FormattedString.mk_str_line) |> FormattedString.mappend |> FormattedString.mk_block |> FormattedString.(<>) (FormattedString.mk_str_line ("Stabilization-Threshold for: " ^ (Atom.to_string ~pretty:true atom))));)

let get_bound t order npe varmap =
  let bound, max_con =
      List.fold_right (fun atom (bound, const) ->
          let poly = Atom.poly atom |> Polynomial.neg in
          let sub_poly, l_ = PE.substitute varmap poly |> PE.remove_frac |> PE.monotonic_kernel (TWNLoop.invariant t |> Formula.mk) (TWNLoop.guard t) in
          let l_max = if List.is_empty [] then OurInt.zero else List.map (fun (x,y) -> monotonicity_th_int 1 x y) l_  |> OurInt.max_list in
          Logger.log logger Logger.INFO (fun () -> "complexity: npe -> guard_atom", ["atom", (Atom.to_string atom); "subs", "0 <= " ^ (PE.to_string sub_poly)]);
          let sub_poly_n = sub_poly |> List.map (fun (c,p,d,b) -> (c, RationalPolynomial.normalize p , d |> OurInt.of_int, b |> OurInt.of_int)) in
          let max_const = OurInt.max_list [const; (PE.max_const sub_poly); l_max] in
            (Bound.add bound ((compute_f atom sub_poly_n)), max_const))
            (TWNLoop.guard_without_inv t |> Formula.atoms |> List.unique ~eq:Atom.equal) (Bound.one, OurInt.zero) in
            Logger.log logger Logger.INFO (fun () -> "complexity.get_bound", ["max constant in constant constraint", (OurInt.to_string max_con)]);
  Bound.(add bound (of_constant (OurInt.add max_con OurInt.one)))
  |> tap (fun b -> Logger.log logger Logger.INFO (fun () -> "complexity.get_bound", ["local bound", Bound.to_string b]))

(* For Testing *)
let check_twn loop =
  (check_weakly_monotonicity loop) && ((List.length (check_triangular loop)) == (VarSet.cardinal ((TWNLoop.input_vars loop))))

let check_twn (_,t,_) = check_twn (TWNLoop.mk_transition t)

let complexity loop =
    let order = check_triangular loop in
    let t_, was_negative =
      if (check_weakly_negativitiy loop) then
        chain loop |> tap (fun loop -> Logger.log logger Logger.INFO (fun () -> "negative", ["chained", TWNLoop.to_string loop])), true
      else loop, false in
    Logger.log logger Logger.INFO (fun () -> "order", ["order", Util.enum_to_string Var.to_string (List.enum order)]);
    proof_append (FormattedString.mk_str_line ("  order: " ^ (Util.enum_to_string (Var.to_string ~pretty:true) (List.enum order))));
    let pe = PE.compute_closed_form (List.map (fun var ->
        let update_var = TWNLoop.update t_ var in
        (var, if Option.is_some update_var then Option.get update_var else Polynomial.of_var var)) order) in
        Logger.log logger Logger.INFO (fun () -> "closed-form", (List.combine (List.map Var.to_string order) (List.map PE.to_string pe)));
        proof_append (
          FormattedString.(mk_str "closed-form:" <> (
          (List.combine (List.map (Var.to_string ~pretty:true) order) (List.map PE.to_string_pretty pe))
          |> List.map (fun (a,b) -> a ^ ": " ^ b)
          |> List.map (FormattedString.mk_str_line) |> FormattedString.mappend |> FormattedString.mk_block)));
    let npe = PE.normalize pe in
        Logger.log logger Logger.INFO (fun () -> "constrained-free closed-form", (List.combine (List.map Var.to_string order) (List.map PE.to_string npe)));
    let varmap = Hashtbl.of_list (List.combine order npe) in
    let terminating = termination_ t_ order npe varmap in
    if not terminating then
      Bound.infinity
    else
      let f = get_bound t_ order npe varmap in if was_negative then Bound.(add (add f f) one) else f

let complexity_ (_,t,_) = complexity (TWNLoop.mk_transition t)

(* CYLES and corresponding time-bound. *)

type path = (Location.t * TWNLoop.t * Location.t) list

(* Computes all cycles containing l0. The function call "cycles l0 trans l0 [l0 ->_t l1, l_1] []" returns all paths containing t *)
let rec cycles trans l0 (paths: (path * LocationSet.t) list) (res: path list) =
  if List.is_empty paths then res
  else
    let (path, loc_done) = List.first paths in
    let path_trans = Set.filter
      (fun (l,_,l') -> let (l_path,_,l_path') = List.first path in Location.equal l l_path' && not (LocationSet.mem l' loc_done)) trans in
    let cycle_trans = Set.filter (fun (_,_,l') -> Location.equal l' l0) path_trans in
    cycles trans l0
      ((List.tl paths) @ (List.map (fun (l,t,l') -> ((l,t,l')::path, LocationSet.add l loc_done)) ((Set.diff path_trans cycle_trans) |> Set.to_list)))
      res @ (List.map (fun t -> (t::path)|> List.rev) (Set.to_list cycle_trans))

(* We compute the transitions l->l by contracting a (shifted to start) cycle (l1 ->_t1 l2 ->_t2 ... -> ln ->_tn l1 with the update tn % ... % t_1
and the guard g1 && g1(t1) && g2(t2 % t1) && .. and cost)  *)
let compose_transitions cycle start =
  let rec split i = function
    | [] -> raise Not_found
    | x::xs ->
        if i == 0 then ([],x,xs) else let y,t,z = split (i - 1) xs in (x::y,t, z)
  in
  let pre, t1, post = split start cycle in
  List.fold (fun u t -> TWNLoop.append u (Tuple3.second t)) (Tuple3.second t1) (post@pre)

(* Finds entered location on cycle. *)
let rec find l list =
    match list with
    | [] -> raise Not_found
    | (l',_,_)::xs -> if Location.equal l l' then 0 else 1 + (find l xs)

(* Checks for a list of cycles if twn synt. req. are fulfilled (we do not (!) check termination here) *)
let find_cycle appr program (cycles: path list) = List.find (fun cycle ->
    let handled_transitions = List.fold (fun xs (l,twn,l') -> (List.map (fun t -> (l,t,l')) (TWNLoop.subsumed_transitionlabels twn))@xs) [] cycle in
    let entries = Program.entry_transitions logger program handled_transitions in
    let twn_loops = List.map (fun (_,_,l') -> compose_transitions cycle (find l' cycle)) entries in
    List.for_all (fun (entry, t) ->
      let eliminated_t =
        EliminateNonContributors.eliminate_t
          (TWNLoop.input_vars t) (TWNLoop.Guard.vars @@ TWNLoop.guard t) (TWNLoop.update t) (TWNLoop.remove_non_contributors t)
      in
      not (VarSet.is_empty (TWNLoop.vars eliminated_t)) (* Are there any variables *)
       && VarSet.equal (TWNLoop.vars eliminated_t) (TWNLoop.input_vars eliminated_t) (* No Temp Vars? *)
       && let order = check_triangular eliminated_t in List.length order == VarSet.cardinal (TWNLoop.input_vars eliminated_t) (* Triangular?*)
       && check_weakly_monotonicity eliminated_t (* Weakly Monotonic? *)
       && List.for_all (Approximation.is_time_bounded appr) entries) (List.combine entries twn_loops)) cycles

       (* TODO: Maybe we should sort w.r.t size-bounds of entry transitions first and take minimum afterwards. And if we get different cycles for the same transitions at different timepoints then we need to compute termination and sth. twice  *)

let rec parallel_edges ys = function
  | [] -> ys
  | (l,t,l')::xs -> let f (l1,loop,l1') = Location.equal l l1 && Location.equal l' l1' && String.equal (TransitionLabel.update_to_string_rhs t) (TWNLoop.update_to_string_rhs loop) in
                    let h (l1,t1,l1') = Location.equal l l1 && Location.equal l' l1' && String.equal (TransitionLabel.update_to_string_rhs t) (TransitionLabel.update_to_string_rhs t1) in
  if List.exists f ys then parallel_edges ys xs
  else parallel_edges ((l, TWNLoop.mk_transitions List.(map Tuple3.second (filter h ((l,t,l')::xs))), l')::ys) xs

module TimeBoundTable = Hashtbl.Make(Transition)

let time_bound_table: ((Transition.t list) * (Transition.t * Bound.t) list) TimeBoundTable.t = TimeBoundTable.create 10

let lift appr entry bound =
  let bound_with_sizebound = Bound.substitute_f (Approximation.sizebound appr entry) bound in
    Bound.mul (Approximation.timebound appr entry) bound_with_sizebound
  |> tap (fun b ->
    proof_append (FormattedString.(mk_paragraph (mk_str_line ("relevant size-bounds w.r.t. t" ^ (Transition.id entry |> Util.natural_to_subscript) ^ ":") <> (
          Bound.vars bound
          |> VarSet.to_list
          |> List.map (fun v -> (Var.to_string ~pretty:true v) ^ ": " ^ (Approximation.sizebound appr entry v |> Bound.to_string ~pretty:true))
          |> List.map (FormattedString.mk_str_line) |> FormattedString.mappend) <>
          FormattedString.mk_str_line ("Runtime-bound of t" ^ (Transition.id entry |> Util.natural_to_subscript) ^ ": " ^ (Approximation.timebound appr entry |> Bound.to_string ~pretty:true)) <>
          FormattedString.mk_str ("Results in: " ^ (Bound.to_string ~pretty:true b))))))

let time_bound (l,t,l') scc program appr = (
  proof := FormattedString.Empty;
  let opt = TimeBoundTable.find_option time_bound_table (l,t,l') in
  if Option.is_none opt then (
    let bound =
      Timeout.timed_run 5. (fun () -> try
        let parallel_edges = parallel_edges [] (TransitionSet.to_list scc) in
        let cycle = find_cycle appr program (
          if Location.equal l l' then
            let f (l1,loop,l1') = Location.equal l l1 && Location.equal l' l1' && String.equal (TransitionLabel.update_to_string_rhs t) (TWNLoop.update_to_string_rhs loop) in
            [[List.find f parallel_edges]]
          else
            (cycles (parallel_edges |> List.filter (fun (l,_,l') -> not (Location.equal l l')) |> Set.of_list) l ([([(l,(TWNLoop.mk_transition t),l')], (LocationSet.singleton l'))]) []))
        in
        let handled_transitions = ListMonad.(cycle >>= fun (l,twn,l') -> TWNLoop.subsumed_transitions l l' twn) in
        let entries = Program.entry_transitions logger program handled_transitions in
        (* add_to_proof_graph program handled_transitions entries; *)
        Logger.log logger Logger.INFO (fun () -> "cycle", ["decreasing", Transition.to_id_string (l,t,l'); "cycle", (TransitionSet.to_id_string (TransitionSet.of_list handled_transitions)); "entry", (TransitionSet.to_id_string (TransitionSet.of_list entries))]);
        let twn_loops = List.map (fun (l,t,l') -> compose_transitions cycle (find l' cycle)) entries in
        Logger.log logger Logger.INFO (fun () -> "twn_loops", List.combine (List.map Transition.to_string entries) (List.map TWNLoop.to_string twn_loops));
        proof_append FormattedString.((mk_header_small (mk_str "TWN-Loops:")) <>
          (List.combine (List.map Transition.to_string_pretty entries) (List.map (TWNLoop.to_string ~pretty:true) twn_loops)
          |> List.map (fun (a,b) -> FormattedString.mk_str_line ("entry: " ^ a) <> FormattedString.mk_block (FormattedString.mk_str_line ("results in twn-loop: " ^ b)))
          |> FormattedString.mappend));
        let global_local_bounds =
          List.map (fun (entry, twn) ->
                let program_only_entry = Program.from ((TransitionSet.to_list (TransitionSet.diff (Program.transitions program) (TransitionSet.of_list entries))) |> List.map List.singleton |> (@) [[entry]]) (Program.start program) in
                let twn_inv = InvariantGeneration.transform_program program_only_entry
                  |> MaybeChanged.unpack
                  |> Program.transitions
                  |> TransitionSet.filter (fun t -> List.exists (Transition.equal t) handled_transitions)
                  |> TransitionSet.to_list
                  |> List.map (TransitionLabel.invariant % Transition.label)
                  |> fun invariants -> List.flatten invariants |> List.filter (fun atom -> List.for_all (List.exists (Atom.equal atom)) invariants)
                  |> TWNLoop.add_invariant twn in

                let eliminated_t =
                  EliminateNonContributors.eliminate_t
                    (TWNLoop.input_vars twn_inv) (TWNLoop.Guard.vars @@ TWNLoop.guard twn_inv) (TWNLoop.update twn_inv) (TWNLoop.remove_non_contributors twn_inv)
                in
                if VarSet.is_empty (TWNLoop.vars eliminated_t) then
                  Bound.infinity, (entry, Bound.infinity)
                else (
                  let bound = complexity eliminated_t in
                  if Bound.is_infinity bound then raise (Non_Terminating (handled_transitions, entries));
                  lift appr entry bound, (entry, bound)) )
          (List.combine entries twn_loops) in
        List.iter (fun t -> TimeBoundTable.add time_bound_table t (handled_transitions, List.map Tuple2.second global_local_bounds)) handled_transitions;
        global_local_bounds |> List.map Tuple2.first |> Bound.sum_list
        with
        | Not_found -> Logger.log logger Logger.DEBUG (fun () -> "twn", ["no twn_cycle found", ""]); Bound.infinity
        | Non_Terminating (handled_transitions,entries)->
            Logger.log logger Logger.DEBUG (fun () -> "twn", ["non terminating", ""]);
            List.iter (fun t -> TimeBoundTable.add time_bound_table t (handled_transitions, List.map (fun t -> (t, Bound.infinity)) entries)) handled_transitions;
            Bound.infinity)
    in
    if Option.is_some bound then
      bound |> Option.get |> Tuple2.first |> tap (fun b -> Logger.log logger Logger.INFO (fun () -> "twn", ["global_bound", Bound.to_string b]))
      |> tap (fun _ -> proof_append FormattedString.((mk_str_line (bound |> Option.get |> Tuple2.first |> Bound.to_string ~pretty:true))))
    else (
      Logger.log logger Logger.INFO (fun () -> "twn", ["Timeout", Bound.to_string Bound.infinity]);
      Bound.infinity)
  )
  else (
    let cycle, xs = Option.get opt in
    let bound_with_sizebound = Bound.sum_list (List.map (fun (entry, bound) -> lift appr entry bound) xs) in
    bound_with_sizebound |> tap (fun b -> Logger.log logger Logger.INFO (fun () -> "twn", ["global_bound", Bound.to_string b]))
                         |> tap (fun b -> proof_append FormattedString.((mk_str_line (b |> Bound.to_string ~pretty:true))))))
  |> tap (fun b -> if Bound.compare_asy b (Approximation.timebound appr (l,t,l')) < 0 then ProofOutput.add_to_proof @@ fun () ->
    FormattedString.((mk_header_big @@ mk_str "Time-Bound by TWN-Loops:") <>
                      mk_header_small (mk_str ("TWN-Loops: t" ^ (TransitionLabel.id t |> Util.natural_to_subscript) ^ " " ^ Bound.to_string ~pretty:true b)) <>
                      !proof))
