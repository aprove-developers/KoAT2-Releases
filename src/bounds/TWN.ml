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

(* let add_to_proof_graph program cycle entries =
  let color_map =
  List.fold_right (fun t -> GraphPrint.TransitionMap.add t GraphPrint.Blue) cycle GraphPrint.TransitionMap.empty 
  |> List.fold_right (fun t -> GraphPrint.TransitionMap.add t GraphPrint.Red) entries in
    proof_append FormattedString.(mk_paragraph (
      match ProofOutput.get_format () with
        | Html -> FormattedString.mk_raw_str (GraphPrint.print_system_pretty_html color_map program)
        | _    -> FormattedString.Empty));
  proof_append (FormattedString.mk_str_line ("  cycle: " ^ (Util.enum_to_string Transition.to_id_string (List.enum cycle)))) *)

(* ELIMINIATE  *)

let depends var label =
    VarSet.exists (fun x -> (TransitionLabel.update label x |? Polynomial.zero |> Polynomial.vars |> VarSet.mem var)
                         || (TransitionLabel.cost label |> Polynomial.vars |> VarSet.mem var))

let rec eliminate_ t contributors non_contributors =
    let (xs,ys) = VarSet.fold (fun y (contr,non_contr) ->
                        if depends y t contr then
                            (VarSet.add y contr, VarSet.remove y non_contr)
                        else
                            (contr,non_contr)) (TransitionLabel.input_vars t)
                        (contributors, non_contributors) in
    if VarSet.equal non_contributors ys then
        contributors
    else
        eliminate_ t xs ys

let eliminate t =
    let vars = TransitionLabel.input_vars t in
    let vars_guard = Constraint.vars (TransitionLabel.guard t)
    and vars_cost = Polynomial.vars (TransitionLabel.cost t) in
    let init_contr = VarSet.union vars_guard vars_cost in
    Logger.(log logger INFO (fun () -> "EliminateNonContributors", [("init_contr", VarSet.to_string init_contr);("init_non_contributors", VarSet.to_string (VarSet.diff vars vars_guard))]));
    let contributors = eliminate_ t init_contr (VarSet.diff vars init_contr) in
    let non_contributors = VarSet.diff vars contributors in
    TransitionLabel.remove_non_contributors non_contributors t

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

let check_triangular (t: TransitionLabel.t) = 
  let vars = VarSet.to_list (TransitionLabel.input_vars t) in 
  let n = List.length vars in
  let vars_i = List.combine vars (List.range 0 `To (n - 1)) in
  let graph = List.mapi (fun i var -> 
    let vars_update = 
        TransitionLabel.update t var 
        |? Polynomial.zero 
        |> Polynomial.vars 
        |> VarSet.remove var 
        |> VarSet.to_list 
        |> List.map (fun var -> List.assoc var vars_i) in (i, vars_update)) vars in
  let order = try toposort graph with CycleFound _ -> [] in
  List.map (fun i -> List.assoc i (List.map Tuple2.swap vars_i)) order
  |> List.rev


(* MONOTONICITY *)

let check_weakly_monotonicity (t: TransitionLabel.t) = 
  VarSet.for_all (fun var -> let update = TransitionLabel.update t var in
                          if Option.is_none update then
                            true
                          else 
                            update |> Option.get |> Polynomial.var_only_linear var) (TransitionLabel.input_vars t)


(* NEGATIVITY *)

let check_weakly_negativitiy (t: TransitionLabel.t) = 
  VarSet.exists (fun var -> let update = TransitionLabel.update t var in
                          if Option.is_none update then
                            false
                          else 
                            update |> Option.get |> Polynomial.coeff_of_var var |> OurInt.is_negative) (TransitionLabel.input_vars t)


let chain (t: TransitionLabel.t) = TransitionLabel.append t t

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

(* For Testing *)
let termination t = 
  let order = check_triangular t in
  let pe = PE.compute_closed_form (List.map (fun var -> 
      let update_var = TransitionLabel.update t var in
      (var, if Option.is_some update_var then Option.get update_var else Polynomial.of_var var)) order) in
  let npe = PE.normalize pe in
  let varmap = Hashtbl.of_list (List.combine order npe) in
  let formula = 
      List.fold_right (fun atom formula -> 
          let poly = Atom.poly atom in 
          let sub_poly = PE.substitute varmap poly |> PE.remove_frac |> List.map (RationalPolynomial.normalize % Tuple4.second) in
          let formula_poly = if Atom.is_lt atom 
              then sub_poly |> red_lt
              else sub_poly |> red_le in Formula.mk_and formula formula_poly) 
              (TransitionLabel.guard_without_inv t) (Formula.mk_true) in
  (not % SMTSolver.satisfiable) (Formula.implies (Formula.mk (TransitionLabel.invariant t)) formula |> Formula.simplify)

let termination_ t order pe npe varmap = 
  let formula = 
      List.fold_right (fun atom formula -> 
          let poly = Atom.poly atom in 
          let sub_poly = PE.substitute varmap poly |> PE.remove_frac |> List.map (RationalPolynomial.normalize % Tuple4.second) in
          let formula_poly = if Atom.is_lt atom 
              then sub_poly |> red_lt
              else sub_poly |> red_le in Formula.mk_and formula formula_poly |> Formula.simplify) 
              (TransitionLabel.guard_without_inv t) (Formula.mk_true) in
  (not % SMTSolver.satisfiable) (Formula.mk_and formula (Formula.mk (TransitionLabel.invariant t)))
  |> tap (fun bool -> Logger.log logger Logger.INFO (fun () -> "termination", ["is_satisfiable", Bool.to_string bool]);
                      Logger.log logger Logger.DEBUG (fun () -> "termination", ["formula", Formula.to_string formula]);
    proof_append
        FormattedString.(
        mk_str_line ("Termination: " ^ (string_of_bool bool))
        <> mk_str_line "Formula: "
        <> (Formula.to_string_pretty formula |> mk_block)
        |> mk_paragraph);)

(* COMPLEXITY: *)

(* Computes the monotoncitiy threshold for (b1,a1) > (b2,a2), i.e., smallest m s.t for all n >= m: n^a1 * b1^n > k * n^a1 * b1^n *)
let monotonicity_th (b1,a1) (b2,a2) k =
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

let compute_kmax sub_poly = sub_poly |> List.map (OurInt.max_list % (List.map OurInt.abs) % Polynomial.coeffs) |> OurInt.max_list

let compute_m sub_poly = (List.map Polynomial.degree sub_poly) |> List.max |> OurInt.of_int

let compute_N = function 
  | [] -> OurInt.zero
  | x::[] -> OurInt.zero
  | x::y::[] -> OurInt.one
  | x1::x2::x3::xs ->
    let mt = List.map (fun x -> monotonicity_th x3 x OurInt.one) xs |> OurInt.max_list in
    let mt_ = monotonicity_th x2 x3 (OurInt.of_int ((List.length xs) + 1)) in
    OurInt.max mt mt_

let compute_M = function 
  | [] -> OurInt.zero
  | x::[] -> OurInt.zero
  | (b1,a1)::(b2,a2)::xs when ((OurInt.is_gt b1 b2) || ((OurInt.equal b1 b2) && (OurInt.is_gt a1 OurInt.(add a2 one)))) 
                         -> monotonicity_th (b1,a1) (b2, OurInt.(add a2 one)) OurInt.one
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
    let alphas = List.map (Tuple4.second) xs in
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
          let sub_poly = PE.substitute varmap poly |> PE.remove_frac in 
          Logger.log logger Logger.INFO (fun () -> "complexity: npe -> guard_atom", ["atom", (Atom.to_string atom); "subs", "0 <= " ^ (PE.to_string sub_poly)]);
          let sub_poly_n = sub_poly |> List.map (fun (c,p,d,b) -> (c, RationalPolynomial.normalize p , d |> OurInt.of_int, b |> OurInt.of_int)) in
          let max_const = OurInt.max const (PE.max_const sub_poly) in
            (Bound.add bound ((compute_f atom sub_poly_n)), max_const))
            (TransitionLabel.guard_without_inv t |> Constraint.simplify) (Bound.one, OurInt.zero) in
            Logger.log logger Logger.INFO (fun () -> "complexity.get_bound", ["max constant in constant constraint", (OurInt.to_string max_con)]);
  Bound.(add bound (of_constant (OurInt.add max_con OurInt.one)))
  |> tap (fun b -> Logger.log logger Logger.INFO (fun () -> "complexity.get_bound", ["local bound", Bound.to_string b]))

(* For Testing *)
let check_twn (l,t,l') =
    (check_weakly_monotonicity t) && ((List.length (check_triangular t)) == (VarSet.cardinal ((TransitionLabel.input_vars t))))

let complexity t = 
    let order = check_triangular t in
    let t_, was_negative =
      if (check_weakly_negativitiy t) then
        chain t |> tap (fun t -> Logger.log logger Logger.INFO (fun () -> "negative", ["chained", TransitionLabel.to_string t])), true
      else t, false in
    Logger.log logger Logger.INFO (fun () -> "order", ["order", Util.enum_to_string Var.to_string (List.enum order)]);
    proof_append (FormattedString.mk_str_line ("  order: " ^ (Util.enum_to_string (Var.to_string ~pretty:true) (List.enum order))));
    let pe = PE.compute_closed_form (List.map (fun var -> 
        let update_var = TransitionLabel.update t_ var in
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
    let terminating = termination_ t_ order pe npe varmap in
    if not terminating then 
      Bound.infinity
    else 
      let f = get_bound t_ order npe varmap in if was_negative then Bound.(add (add f f) one) else f


(* CYLES and corresponding time-bound. *)

type path = Transition.t list

(* Computes all cycles containing l0. The function call "cycles l0 trans l0 [l0 ->_t l1, l_1] []" returns all paths containing t *)
let rec cycles trans l0 (paths: (path * LocationSet.t) list) (res: path list) =
  if List.is_empty paths then res
  else 
    let (path, loc_done) = List.first paths in
    let path_trans = TransitionSet.filter 
      (fun (l,t,l') -> let (l_path,_,l_path') = List.first path in Location.equal l l_path' && not (LocationSet.mem l' loc_done)) trans in
    let cycle_trans = TransitionSet.filter (fun (l,t,l') -> Location.equal l' l0) path_trans in
    cycles trans l0
      ((List.tl paths) @ (List.map (fun (l,t,l') -> ((l,t,l')::path, LocationSet.add l loc_done)) ((TransitionSet.diff path_trans cycle_trans) |> TransitionSet.to_list)))
      res @ (List.map (fun t -> (t::path)|> List.rev) (TransitionSet.to_list cycle_trans))

(* We compute the transitions l->l by contracting a (shifted to start) cycle (l1 ->_t1 l2 ->_t2 ... -> ln ->_tn l1 with the update tn % ... % t_1
and the guard g1 && g1(t1) && g2(t2 % t1) && .. and cost TODO )  *)
let compose_transitions cycle start = 
  let rec split i = function
  | [] -> raise Not_found
  | x::xs -> if i == 0 then ([],x,xs) else let y,t,z = split (i - 1) xs in (x::y,t, z)
  |> Tuple3.map1 List.rev in
  let pre, t1, post = split start (List.rev cycle |> List.map Tuple3.second) in 
  List.fold (fun u t -> TransitionLabel.append t u) t1 (pre@post)

let rec find l list =
    match list with
    | [] -> raise Not_found
    | (l',_,_)::xs -> if Location.equal l l' then 0 else 1 + (find l xs)

(* Checks for a list of cycles if twn synt. req. are fulfilled (we do not (!) check termination here) *)
let find_cycle appr program cycles = List.find (fun cycle -> 
    let entries = Program.entry_transitions logger program cycle in
    let twn_loops = List.map (fun (l,t,l') -> compose_transitions cycle (find l' cycle)) entries in
    List.for_all (fun (entry, t) -> 
      let eliminated_t = TransitionLabel.without_inv t |> eliminate in
      not (VarSet.is_empty (TransitionLabel.vars eliminated_t)) (* Are there any variables *)
       && VarSet.equal (TransitionLabel.vars eliminated_t) (TransitionLabel.input_vars eliminated_t) (* No Temp Vars? *)
       && let order = check_triangular eliminated_t in (List.length order) == (VarSet.cardinal ((TransitionLabel.input_vars eliminated_t))) (* Triangular?*)
       && check_weakly_monotonicity eliminated_t (* Weakly Monotonic? *)
       && List.for_all (Approximation.is_time_bounded appr) entries) (List.combine entries twn_loops)) cycles

       (* TODO: Maybe we should sort w.r.t size-bounds of entry transitions first and take minimum afterwards. And if we get different cycles for the same transitions at different timepoints then we need to compute termination and sth. twice  *)

module TimeBoundTable = Hashtbl.Make(Transition) 

let time_bound_table: (path * (Transition.t * Bound.t) list) TimeBoundTable.t = TimeBoundTable.create 10

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
  try 
  let opt = TimeBoundTable.find_option time_bound_table (l,t,l') in 
  if Option.is_none opt then (
    let bound = 
    Timeout.timed_run 5. ~action:(fun () -> ()) (fun () -> 
      let cycle = find_cycle appr program (if Location.equal l l' then [[(l,t,l')]] else (cycles scc l ([([(l,t,l')], (LocationSet.singleton l'))]) [])) in
      let entries = Program.entry_transitions logger program cycle in
      (* add_to_proof_graph program cycle entries; *)
      Logger.log logger Logger.INFO (fun () -> "cycle", ["decreasing", Transition.to_id_string (l,t,l'); "cycle", (TransitionSet.to_id_string (TransitionSet.of_list cycle)); "entry", (TransitionSet.to_id_string (TransitionSet.of_list entries))]);
      let twn_loops = List.map (fun (l,t,l') -> compose_transitions cycle (find l' cycle)) entries in
      Logger.log logger Logger.INFO (fun () -> "twn_loops", List.combine (List.map Transition.to_string entries) (List.map TransitionLabel.to_string twn_loops));
          proof_append FormattedString.((mk_header_small (mk_str "TWN-Loops:")) <>  
          (List.combine (List.map Transition.to_string_pretty entries) (List.map (TransitionLabel.to_string ~pretty:true) twn_loops)
          |> List.map (fun (a,b) -> FormattedString.mk_str_line ("entry: " ^ a) <> FormattedString.mk_block (FormattedString.mk_str_line ("results in twn-loop: " ^ b)))
          |> FormattedString.mappend));
        let global_local_bounds =
          List.map (fun (entry, t) -> 
              let eliminated_t = TransitionLabel.without_inv t |> eliminate in
                if VarSet.is_empty (TransitionLabel.vars eliminated_t) then 
                  Bound.infinity, (entry, Bound.infinity) 
                else
                  let bound = complexity eliminated_t in
                  if Bound.is_infinity bound then raise (Non_Terminating (cycle, entries));
                  lift appr entry bound, (entry, bound))
          (List.combine entries twn_loops) in
        List.iter (fun t -> TimeBoundTable.add time_bound_table t (cycle, List.map Tuple2.second global_local_bounds)) cycle;
        global_local_bounds |> List.map Tuple2.first |> Bound.sum_list) in
      if Option.is_some bound then
        bound |> Option.get |> Tuple2.first |> tap (fun b -> Logger.log logger Logger.INFO (fun () -> "twn", ["global_bound", Bound.to_string b]))
        |> tap (fun _ -> proof_append FormattedString.((mk_str_line (bound |> Option.get |> Tuple2.first |> Bound.to_string ~pretty:true))))
      else (
        Logger.log logger Logger.INFO (fun () -> "twn", ["Timeout", Bound.to_string Bound.infinity]);
        Bound.infinity)
  )
  else
    let cycle, xs = Option.get opt in
    let bound_with_sizebound = Bound.sum_list (List.map (fun (entry, bound) -> lift appr entry bound) xs) in
    bound_with_sizebound |> tap (fun b -> Logger.log logger Logger.INFO (fun () -> "twn", ["global_bound", Bound.to_string b]))
                         |> tap (fun b -> proof_append FormattedString.((mk_str_line (b |> Bound.to_string ~pretty:true)))) 
  with 
    | Not_found -> Logger.log logger Logger.DEBUG (fun () -> "twn", ["no twn_cycle found", ""]); Bound.infinity
    | Non_Terminating (cycle,entries)-> 
      Logger.log logger Logger.DEBUG (fun () -> "twn", ["non terminating", ""]); 
      List.iter (fun t -> TimeBoundTable.add time_bound_table t (cycle, List.map (fun t -> (t, Bound.infinity)) entries)) cycle;
      Bound.infinity)
  |> tap (fun b -> if Bound.compare_asy b (Approximation.timebound appr (l,t,l')) < 0 then ProofOutput.add_to_proof @@ fun () -> 
    FormattedString.((mk_header_big @@ mk_str "Time-Bound by TWN-Loops:") <> 
                      mk_header_small (mk_str ("TWN-Loops: t" ^ (TransitionLabel.id t |> Util.natural_to_subscript) ^ " " ^ Bound.to_string ~pretty:true b)) <> 
                      !proof))