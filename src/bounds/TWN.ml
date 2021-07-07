open Batteries
open Polynomials
open ProgramTypes
open Formulas
open Atoms
open BoundsInst
open Constraints
open PolyExponential

(*TODO *)
let logger = Logging.(get Twn)

(*TODO Cost, negative coeff.  *)

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

(* TERMINATION: *)

module SMTSolver = SMT.Z3Opt

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
              (TransitionLabel.guard t) (Formula.mk_true) in
  (not % SMTSolver.satisfiable) formula

let termination_ t order pe npe varmap = 
  let formula = 
      List.fold_right (fun atom formula -> 
          let poly = Atom.poly atom in 
          let sub_poly = PE.substitute varmap poly |> PE.remove_frac |> List.map (RationalPolynomial.normalize % Tuple4.second) in
          let formula_poly = if Atom.is_lt atom 
              then sub_poly |> red_lt
              else sub_poly |> red_le in Formula.mk_and formula formula_poly) 
              (TransitionLabel.guard t) (Formula.mk_true) in
  (not % SMTSolver.satisfiable) formula
  |> tap (fun bool -> Logger.log logger Logger.INFO (fun () -> "termination", ["is_satisfiable", Bool.to_string bool]);
                      Logger.log logger Logger.DEBUG (fun () -> "termination", ["formula", Formula.to_string formula]))


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

let compute_f = function
  | [] -> Bound.zero
  | x::[] -> Bound.zero
  | xs -> 
    let alphas = List.map (Tuple4.second) xs in
    Logger.log logger Logger.INFO (fun () -> "complexity.compute_f", ["alphas", (alphas |> List.enum |> Util.enum_to_string Polynomial.to_string)]);
    let alphas_abs = compute_alpha_abs alphas in
    Logger.log logger Logger.INFO (fun () -> "complexity.compute_f", ["alphas_abs", Polynomial.to_string alphas_abs]);
    let base_exp = List.map (fun (_,_,d,b) -> (b,d)) xs in
    let n_ = compute_N base_exp in
    Logger.log logger Logger.INFO (fun () -> "complexity.compute_f", ["N", (OurInt.to_string n_)]);
    let m_ = compute_M base_exp  in
    Logger.log logger Logger.INFO (fun () -> "complexity.compute_f", ["M", (OurInt.to_string m_)]);
    Bound.(of_poly (Polynomial.add alphas_abs alphas_abs) |> add (of_constant (OurInt.max m_ n_)))
    |> tap (fun b -> Logger.log logger Logger.INFO (fun () -> "complexity.compute_f", ["2*alpha_abs+max(N,M)", Bound.to_string b]))

let get_bound t order npe varmap = 
  let bound, max_con = 
      List.fold_right (fun atom (bound, const) -> 
          let poly = Atom.poly atom |> Polynomial.neg in 
          let sub_poly = PE.substitute varmap poly |> PE.remove_frac in 
          Logger.log logger Logger.INFO (fun () -> "complexity: npe -> guard_atom", ["atom", (Atom.to_string atom); "subs", "0 <= " ^ (PE.to_string sub_poly)]);
          let sub_poly_n = sub_poly |> List.map (fun (c,p,d,b) -> (c, RationalPolynomial.normalize p , d |> OurInt.of_int, b |> OurInt.of_int)) in
          let max_const = OurInt.max const (PE.max_const sub_poly) in
          let rec summand = function
            | [] -> Bound.zero
            | x::[] -> Bound.zero
            | x::xs -> Bound.add (compute_f (x::xs)) (summand xs) in
            (Bound.add bound ((summand sub_poly_n)), max_const))
            (TransitionLabel.guard t) (Bound.one, OurInt.zero) in
            Logger.log logger Logger.INFO (fun () -> "complexity.get_bound", ["max constant in constant constraint", (OurInt.to_string max_con)]);
  Bound.(add bound (of_constant (OurInt.add max_con OurInt.one)))
  |> tap (fun b -> Logger.log logger Logger.INFO (fun () -> "complexity.get_bound", ["local bound", Bound.to_string b]))

let check_twn (l,t,l') =
    (check_weakly_monotonicity t) && ((List.length (check_triangular t)) == (VarSet.cardinal ((TransitionLabel.input_vars t))))

let complexity t = 
  let order = check_triangular t in
  if not (VarSet.is_empty (VarSet.diff (TransitionLabel.vars t) (TransitionLabel.input_vars t))) then
    Bound.infinity  (* Non-det. is not allowed *)
  else if (List.length order) != (VarSet.cardinal ((TransitionLabel.input_vars t))) then
    Bound.infinity (* Not triangular *)
  else if not (check_weakly_monotonicity t) then
    Bound.infinity (* Not weakly monotonic *)
  else (
    Logger.log logger Logger.INFO (fun () -> "order", ["order", Util.enum_to_string Var.to_string (List.enum order)]);
    let pe = PE.compute_closed_form (List.map (fun var -> 
        let update_var = TransitionLabel.update t var in
        (var, if Option.is_some update_var then Option.get update_var else Polynomial.of_var var)) order) in
        Logger.log logger Logger.INFO (fun () -> "closed-form", (List.combine (List.map Var.to_string order) (List.map PE.to_string pe)));
    let npe = PE.normalize pe in
        Logger.log logger Logger.INFO (fun () -> "constrained-free closed-form", (List.combine (List.map Var.to_string order) (List.map PE.to_string npe)));
    let varmap = Hashtbl.of_list (List.combine order npe) in
    let terminating = termination_ t order pe npe varmap in
    if not terminating then 
      Bound.infinity
    else 
      get_bound t order npe varmap)

(* Cycles and corresponding time-bound. *)

module DjikstraTransitionGraph = Graph.Path.Dijkstra(TransitionGraph)(TransitionGraphWeight(OurInt))

let getTransitionGraph (trans: TransitionSet.t)  =
  TransitionGraph.empty
  |> TransitionSet.fold (fun transition graph -> 
        transition
        |> TransitionGraph.add_edge_e graph) trans


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

module TimeBoundTable = Hashtbl.Make(Transition) 

let time_bound_table: Bound.t TimeBoundTable.t = TimeBoundTable.create 10

let time_bound (l,t,l') scc program appr = 
  let opt = TimeBoundTable.find_option time_bound_table (l,t,l') in
  if Option.is_none opt then 
    let twn_scc = TransitionSet.filter check_twn scc in
    let graph = getTransitionGraph twn_scc in
    let path, _ = DjikstraTransitionGraph.shortest_path graph l' l in
    let cycle = ((l,t,l')::path) in
    let entry = Program.entry_transitions logger program cycle in
    Logger.log logger Logger.INFO (fun () -> "cycle", ["decreasing", Transition.to_id_string (l,t,l'); "cycle", (TransitionSet.to_id_string (TransitionSet.of_list cycle)); "entry", (TransitionSet.to_id_string (TransitionSet.of_list entry))]);
    let twn_loops = List.map (fun (l,t,l') -> compose_transitions cycle (find l' cycle)) entry in
    Logger.log logger Logger.INFO (fun () -> "twn_loops", List.combine (List.map Transition.to_string entry) (List.map TransitionLabel.to_string twn_loops));
    let bound = List.fold_right (fun (entry, t) b -> Bound.(add b (mul (Approximation.timebound appr entry) (complexity t)))) (*TODO: Stop if one is inf; TODO add bound for all tran on cycle*)
        (List.combine entry twn_loops) 
        Bound.zero in
    List.iter (fun t -> TimeBoundTable.add time_bound_table t bound) cycle;
    bound
    |> tap (fun b -> Logger.log logger Logger.INFO (fun () -> "time_bound", ["global_bound", Bound.to_string b]))
else Option.get opt
  |> tap (fun b -> Logger.log logger Logger.INFO (fun () -> "time_bound", ["global_bound", Bound.to_string b]))