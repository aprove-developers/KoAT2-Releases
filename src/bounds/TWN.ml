open Batteries
open Polynomials
open ProgramTypes
open Formulas
open Atoms
open BoundsInst
open Constraints
open PolyExponential
open Lacaml.D
open TransitionLabel

(* PROOF *)
let logger = Logging.(get Twn)

let proof = ref FormattedString.Empty

let proof_append f_str = proof := FormattedString.(!proof <> f_str)

let add_to_proof_graph program cycle entries =
  let a = Mat.random ~from:(-500.) ~range:1000. 5 5 in
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

let check_triangular_t (t: TransitionLabel.t) = check_triangular (TWNLoop.mk_transition t)

let check_solvable (t: TWNLoop.t) =
  let module DG = Graph.Persistent.Digraph.ConcreteBidirectional(Var) in
  let module SCC = Graph.Components.Make(DG) in
  let dg_linear = VarSet.fold (fun x graph ->
              let vars = TWNLoop.update t x |? Polynomial.of_var x |> Polynomial.vars in
              VarSet.fold (fun y graph -> DG.add_edge graph x y) vars graph) (TWNLoop.vars t) DG.empty and
  dg_non_linear = VarSet.fold (fun x graph ->
              let update = TWNLoop.update t x |? Polynomial.of_var x in
              let linear_vars = update
                |> Polynomial.vars
                |> VarSet.filter (fun v -> Polynomial.var_only_linear v update |> not) in
              VarSet.fold (fun y graph -> DG.add_edge graph x y) linear_vars graph) (TWNLoop.vars t) DG.empty in
  let blocks = SCC.scc_list dg_linear in
  if List.for_all (fun scc -> List.length scc = 1) (SCC.scc_list dg_non_linear) (* We don't have cyclic non-linear dependencies. *)
  && List.for_all (fun scc ->
                    List.for_all (fun x ->
                      List.for_all (fun y ->
                      TWNLoop.update t x
                      |? Polynomial.of_var x
                      |> Polynomial.var_only_linear y)
                      scc)
                    scc) blocks (* For all blocks, all variables x,y in such a block: The update of x only depends linear on y *) then
    Option.some (blocks)
  else
    None

let check_solvable_t (t: TransitionLabel.t) = check_solvable (TWNLoop.mk_transition t)



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
  print_string "244 TWN";
  (check_weakly_monotonicity loop) && ((List.length (check_triangular loop)) == (VarSet.cardinal ((TWNLoop.input_vars loop))))

let check_twn (_,t,_) =
  print_string "248TWN \n"; check_twn (TWNLoop.mk_transition t)

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

(* get_linear_update_of_variable (x<- 2x+3y+y^2) x y returns 3 *)
let get_linear_update_of_variable (t:TransitionLabel.t) (var_left:TWNLoop.VarMap.key) (var_right:TWNLoop.VarMap.key)=
  let update = Option.get (TransitionLabel.update t var_left) in
  let fupdate = List.filter (fun (x,y) -> Monomial.is_univariate_linear y) (Polynomial.monomials_with_coeffs update) in
  let (x,y) = List.find  (fun (x,y) -> (Var.equal (List.first (VarSet.to_list (Monomial.vars y))))  var_right) fupdate in
  x
(* get_linear_update_list (x<- 2x+3y+y^2) x [x;y] returns [2;3] *)
let rec get_linear_update_list (t:TransitionLabel.t) (var_left:TWNLoop.VarMap.key) (block:TWNLoop.VarMap.key list) = match block with
  | [] -> []
  | x::xs -> get_linear_update_of_variable t var_left x :: get_linear_update_list t var_left xs

let matrix_of_linear_assignments (t:TransitionLabel.t) (block:TWNLoop.VarMap.key list) =
  List.map (fun x -> get_linear_update_list t x block) block


(*TWNLoop.VarMap.key list  option -> bool *)
let check_transformable_one_block (block:TWNLoop.VarMap.key list) = match block with
  | [] -> false
  | [x] -> true
  | (x::xs) -> true (* TODO: this case where the magic happens *)

(* For each block we check if it is transformable *)
(*TWNLoop.VarMap.key list list option -> bool list *)
let rec check_transformable (blocks:TWNLoop.VarMap.key list list option) = match blocks with
  | None -> []
  | Some [] -> []
  | Some (xs::xss) -> (check_transformable_one_block xs)::(check_transformable (Some xss))


(* Finds entered location on cycle. *)
let rec find l list =
    match list with
    | [] -> raise Not_found (*TODO umbenennen *)
    | (l',_,_)::xs -> if Location.equal l l' then 0 else 1 + (find l xs)

(* Checks for a list of cycles if twn synt. req. are fulfilled (we do not (!) check termination here) *)
let find_cycle_og appr program (cycles: path list) =
  print_string "\n 316 TWN";
  List.find (fun cycle ->
    (* list of all transitions in a cycle, twnloop doesn't require each transition to be in twn form, but together they do *)
    let handled_transitions = List.fold (fun xs (l,twn,l') -> (List.map (fun t -> (l,t,l')) (TWNLoop.subsumed_transitionlabels twn))@xs) [] cycle in
    let entries = Program.entry_transitions logger program handled_transitions in
    let twn_loops = List.map (fun (_,_,l') -> compose_transitions cycle (find l' cycle)) entries in (* 'find' throws an exception *)
    List.for_all (fun (entry, t) ->
      let eliminated_t = (* throw out useless variables *)
        EliminateNonContributors.eliminate_t
          (TWNLoop.input_vars t) (TWNLoop.Guard.vars @@ TWNLoop.guard t) (TWNLoop.update t) (TWNLoop.remove_non_contributors t)
      in
      not (VarSet.is_empty (TWNLoop.vars eliminated_t)) (* Are there any variables *)
       && VarSet.equal (TWNLoop.vars eliminated_t) (TWNLoop.input_vars eliminated_t) (* No Temp Vars? *)
       && let order = check_triangular eliminated_t in List.length order == VarSet.cardinal (TWNLoop.input_vars eliminated_t) (* Triangular?*) (*TODO: automorphism check here *)
       && check_weakly_monotonicity eliminated_t (* Weakly Monotonic? *) (* nicht mehr checken sondern check transformable aufrufen, diese und obere zeile, danach den rückgabewert anpassen *)
       && List.for_all (Approximation.is_time_bounded appr) entries) (List.combine entries twn_loops)) cycles
       (* TODO: Maybe we should sort w.r.t size-bounds of entry transitions first and take minimum afterwards. And if we get different cycles for the same transitions at different timepoints then we need to compute termination and sth. twice  *)
  


let find_cycle appr program (cycles: path list) =
  print_string "\n 317 TWN";
  List.find (fun cycle ->
    (* list of all transitions in a cycle, twnloop doesn't require each transition to be in twn form, but together they do *)
    let handled_transitions = List.fold (fun xs (l,twn,l') -> (List.map (fun t -> (l,t,l')) (TWNLoop.subsumed_transitionlabels twn))@xs) [] cycle in
    let entries = Program.entry_transitions logger program handled_transitions in
    let twn_loops = List.map (fun (_,_,l') -> compose_transitions cycle (find l' cycle)) entries in (* 'find' throws an exception *)
    List.for_all (fun (entry, t) ->
        let eliminated_t = (* throw out useless variables *)
          EliminateNonContributors.eliminate_t
            (TWNLoop.input_vars t) (TWNLoop.Guard.vars @@ TWNLoop.guard t) (TWNLoop.update t) (TWNLoop.remove_non_contributors t)
        in
        not (VarSet.is_empty (TWNLoop.vars eliminated_t)) (* Are there any variables *)
        && VarSet.equal (TWNLoop.vars eliminated_t) (TWNLoop.input_vars eliminated_t) (* No Temp Vars? *)
        && List.for_all (fun x -> x) (check_transformable (check_solvable t)) (* check if all are twn transformable, TODO if thats the case return the transformed and the transformation *)
        && let order = check_triangular eliminated_t in List.length order == VarSet.cardinal (TWNLoop.input_vars eliminated_t) (* Triangular?*) (*TODO: automorphism check here *)
        && check_weakly_monotonicity eliminated_t (* Weakly Monotonic? *) (* nicht mehr checken sondern check transformable aufrufen, diese und obere zeile, danach den rückgabewert anpassen *)
        && List.for_all (Approximation.is_time_bounded appr) entries) 
      (List.combine entries twn_loops)) 
    cycles 
       (* TODO: Maybe we should sort w.r.t size-bounds of entry transitions first and take minimum afterwards. And if we get different cycles for the same transitions at different timepoints then we need to compute termination and sth. twice  *)

let rec parallel_edges ys = function
  | [] -> ys
  | (l,t,l')::xs -> let f (l1,loop,l1') = Location.equal l l1 && Location.equal l' l1' && String.equal (TransitionLabel.update_to_string_rhs t) (TWNLoop.update_to_string_rhs loop) in
                    let h (l1,t1,l1') = Location.equal l l1 && Location.equal l' l1' && String.equal (TransitionLabel.update_to_string_rhs t) (TransitionLabel.update_to_string_rhs t1) in
  if List.exists f ys then parallel_edges ys xs
  else parallel_edges ((l, TWNLoop.mk_transitions List.(map Tuple3.second (filter h ((l,t,l')::xs))), l')::ys) xs

module TimeBoundTable = Hashtbl.Make(Transition)

(* keys: liste von transition (kreise), values: bounds von eingangstransitionen *)
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


let print_flist xs = (
  Printf.printf "[";
  List.iter (Printf.printf "%f; ") xs;
  Printf.printf "]\n";)

let print_vec v = 
  let flist = Lacaml.D.Vec.to_list v in
  print_flist flist

let print_mat m = 
  let m_float = Lacaml.D.Mat.to_list m in
  List.map print_flist m_float

let mat_of_big_int_list m = 
  Lacaml.D.Mat.of_list @@ List.map (List.map Big_int.float_of_big_int) m 

(* TODO: remove if lacaml provides schur composition
let jordan_block eigen_value eigen_vector =
  [5]

let pairwise_distinct xs =
  let sorted_xs = List.sort (fun x y -> if x<y then 1 else -1) xs in 
  let rec pairwise_distinct_help = function 
    x::y::xs -> if x == y then false else pairwise_distinct_help (y::xs)
   |  _ -> true  
  in pairwise_distinct_help sorted_xs  *)

let list_list_to_string xss =
  let yss = List.map (fun xs -> List.map (fun x -> Big_int.to_string x) xs) xss in
  let zs = List.map (fun ys -> List.fold (fun x y -> x ^y^"; ") " " ys) yss in
  List.fold (fun x y -> x^y^"; " ) " " zs

(* matrix is square *)
let transform_linearly_matrix (matrix:Lacaml.D.mat) = 
  if Lacaml.D.Mat.dim1 matrix == 1 
      then (Lacaml.D.Mat.of_list [[1.]], matrix)
  else (
      let _matrix = Lacaml.D.lacpy matrix in (*copy, side-effects would transfer to the function calling this *)
      let (number_of_distinct_eigen_values, real_part_eigen_values, im_part_eigen_values,transformation) = Lacaml.D.gees _matrix in (* matrix is now the transformed *)
      if not @@ List.for_all (fun x -> Float.abs(x) < 0.0001) (Lacaml.D.Vec.to_list im_part_eigen_values) (* TODO find better way of testing complex *)
        then raise Not_found (* TODO maybe option is better than raising exception *)
      (* else if (pairwise_distinct (Lacaml.D.Vec.to_list real_part_eigen_values)) 
        then (Lacaml.D.Mat.of_list [[1.]], (Lacaml.D.Mat.of_diag real_part_eigen_values))*)
      else (
      
      (* let eigen_value_blocks = eigen_values_to_blocks eigen_values in 
      let jordan_blocks = Lacaml.D.Mat.of_diag in 
      let res = merge_jordan_blocks_and_transformations jordan_blocks in 
      let transformed_matrix = Lacaml.D.gemm (Lacaml.D.Mat.transpose_copy transformation) @@ Lacaml.D.gemm og_matrix transformation in *)
      Printf.printf "450: original: \n";print_mat matrix;
      Printf.printf "450: upper triangular:\n";print_mat _matrix;
      Printf.printf "450: orthonormal: \n";print_mat transformation;
      Printf.printf "450: eigenvaleues: \n"; (print_vec real_part_eigen_values);
      (transformation,_matrix)))(*  [[Big_int.of_int 2;Big_int.of_int 0];[Big_int.of_int 0;Big_int.of_int 3]]))*)

let exists_linear_automorphism = function
  | _ -> true

let exists_non_linear_automorphism = function
  | _ -> true

(* unfinished *)
let transform_linearly (cycle: path) =
  print_string "\n 363 TWN";
  cycle

(* unfinished *)
let transform_non_linearly (cycle: path) = print_string "\n 366 TWN"; cycle

(* unfinished: TODO remember the transformation*)
(* transforms only if it's twn transformable, otherwise unchanged *)
let transform (cycle: path) =
  (*if (check_twn cycle) then cycle
  else*) if (exists_linear_automorphism cycle) then (transform_linearly cycle)
  else if (exists_non_linear_automorphism cycle) then (transform_non_linearly cycle)
  else cycle

 let to_string arg =
  if Option.is_some arg then
    "solvable: " ^ (Util.enum_to_string (Util.enum_to_string Var.to_string) (Option.get arg |> List.map List.enum |> List.enum))
  else
    "not solvable" (* Just for Testing *)


(** [matrix_times_vector A x] returns a polynomial list where each element stores a row of [A*x] *)
let matrix_times_vector (matrix:Lacaml__D.mat) (vars:TWNLoop.VarMap.key list) =
  let matrix = List.map (List.map Num.of_float) @@ Lacaml.D.Mat.to_list matrix in (*make mat to list list OurFloat  *)
  List.map (fun xs -> List.fold_left2 (fun acc x var -> RealPolynomial.add acc (RealPolynomial.mult_with_const x (RealPolynomial.of_power var 1))) RealPolynomial.zero xs (vars)) matrix


(** sorts the blocks from function check_solvable in the order defined in the transition (needs O(n^2 log n) due to index_of) *)
let change_order t blocks =
  let var_list = (VarSet.to_list (TransitionLabel.vars t)) in
  let blocks = List.map (List.sort (fun x y ->
                                            if List.index_of x var_list < List.index_of y var_list then -1 else 1)
                        ) blocks in
  List.sort (fun x y -> if List.index_of (List.first x) var_list < List.index_of (List.first y) var_list then -1 else 1) blocks

(** compose_polynomials [x;y] [x^2;y^3] (x <- x+y)  returns x <- x^2+y^3. 
    tail recursive*)
let rec compose_polynomials variables new_polynomials old_poly = 
  let rec compose_polynomials_acc variables new_polynomials old_poly acc_poly = match variables, new_polynomials with 
    | [],[] -> RealPolynomial.add old_poly acc_poly
    | x::xs,poly::polys -> compose_polynomials_acc xs polys old_poly
                                 (RealPolynomial.add acc_poly @@ RealPolynomial.sub (RealPolynomial.substitute x ~replacement:poly old_poly) old_poly) 
    | _,_ -> raise Not_found (* TODO: find better exception for this case or better idea than exception *) in 
  compose_polynomials_acc variables new_polynomials old_poly (RealPolynomial.zero) 

(*  *)
let apply_poly_transformation  variables old_polynomials new_polynomials = 
  List.map (compose_polynomials variables new_polynomials) old_polynomials

let time_bound (l,transition,l') scc program appr = (
  
  
  (* y2 = 3*x^2 +3 *)
  (* y3 = x +y ---- y4 = -2x + 4y;   y5 = -x + y;    y6 = -x - y 
  let y2 = Polynomial.mult_with_const (Big_int.big_int_of_int 3) (Polynomial.add (Polynomial.of_power (Var.of_string "x") 2) (Polynomial.one)) in
  let y4 =  RealPolynomial.add (RealPolynomial.mult_with_const (Num.num_of_int (-2)) (RealPolynomial.of_power (Var.of_string "x") 1)) ((RealPolynomial.mult_with_const (Num.num_of_int (4)) (RealPolynomial.of_power (Var.of_string "y") 1))) in
  let y5 = RealPolynomial.add (RealPolynomial.mult_with_const (Num.num_of_int (-1)) (RealPolynomial.of_power (Var.of_string "x") 1)) ((RealPolynomial.mult_with_const (Num.num_of_int (1)) (RealPolynomial.of_power (Var.of_string "y") 1))) in
  let y6 = RealPolynomial.add (RealPolynomial.mult_with_const (Num.num_of_int (-1)) (RealPolynomial.of_power (Var.of_string "x") 1)) ((RealPolynomial.mult_with_const (Num.num_of_int (-1)) (RealPolynomial.of_power (Var.of_string "y") 1))) in
  let y7 = (RealPolynomial.of_power (Var.of_string "x") 1) in
  let y8 = (RealPolynomial.of_power (Var.of_string "y") 1) in
  let y9 = (RealPolynomial.of_power (Var.of_string "y") 2) in
  let variables_list = [Var.of_string "x"; Var.of_string "y"] in
  let t123 = TransitionLabel.mk ~cost:(TransitionLabel.cost t) ~guard:(TransitionLabel.guard t) ~assignments:[y2] ~patterns:[Var.of_string "x"] ~vars:variables_list in
  Printf.printf "466 %s\n" (TransitionLabel.to_string t);

  Printf.printf "%s\n" (to_string (check_solvable_t t)); (* Just for Testing *)
  (*let matrix = List.map (matrix_of_linear_assignments t) (Option.get (check_solvable_t t)) in*)
  print_string (Big_int.string_of_big_int (get_linear_update_of_variable t (Var.of_string "Arg_1") (Var.of_string "Arg_0")));
  let matrix = matrix_of_linear_assignments t (List.first (Option.get (check_solvable_t t))) in
  Printf.printf "475: %s\n" (list_list_to_string matrix);
  let var_left = List.first (List.first (Option.get (check_solvable_t t))) in
  let update1 = Option.get (TransitionLabel.update t var_left) in
  Printf.printf "475: %s\n" (list_list_to_string matrix);
  let update1 = Polynomial.add update1 update1 in
  let update1 = (Option.get (TransitionLabel.update t var_left)) in
  Printf.printf "484: %s\n" (Polynomial.to_string_pretty update1);

  
  let m1 = Lacaml.D.Mat.of_list [[1.;1.;1.;-1.;0.];[0.;1.;0.;0.;1.];[0.;0.;0.;1.;0.];[0.;0.;-1.;2.;1.];[0.;0.;0.;0.;1.]] in 
  let ( left_eigen_vectors,real_part_eigen_values, im_part_eigen_values,right_eigen_vectors) = Lacaml.D.geev m1 in 
  transpose_copy hat keine seiteneffekte
  *)

  (*now for higher dimensions: *)
  (*Fragen:
  Wie kann ich übersichtlicher coden? and und einrücken
  Wie binde ich Lacaml ein?
  Wieso wird der code doppelt auf dieser Transition ausgeführt? normales fixpunkt-gedöns
  Wann muss man Klammern bei einer Funktion am Anfang schreiben? für tap
  Wie ersetze ich dann die Transition durch die transformierte? nicht ersetzen sondern nur laufzeit mit neuer transition berechnen und die laufzeit wieder zurücktransformieren
  
  Aber sollte man dann nicht die Funktion bound mit der transformierten Transition aufrufen?
  pretty prints funktionieren nicht: egal
  test for complexity/ Rundungsfehler: alles muss int sein  
  Polynome sind hier immer BigInt Polynome? ja, todo probe
  wie sinnvoll ist jordan normalform? reicht nicht eine Transformation in eine obere Dreieckmatrix (Schursche Normalform, diagonal genau dann wenn möglich)

  Wenn die transformation nicht rational nicht, wie bei dem beispiel, wie soll ich dann den Guard anpassen?
  Guard sachen sind mit TODO markiert
  Wie ersetze ich nun die Transition?/wo findet das berechnen der closed form usw statt?

  Also die Jordannormalform (wenn sie reell ist) ist von bestimmten Matrizen algebraisch reell aber nicht rational, siehe [1;2][1;1], von daher 
  sehe ich die gleichen Probleme auftreten auch wenn man diese python bibliothek aufruft. 
  *)

  (* find order for variables and independent blocks and sort them for elegant code when updating transition *)
  let blocks = change_order transition @@ Option.get (check_solvable_t transition) in 
  let concat_blocks = List.concat blocks in 
  (* calculate matrices and cast them to Lacaml.D.mat *)
  let matrices = List.map (mat_of_big_int_list) @@ List.map (matrix_of_linear_assignments transition) blocks in 
  let (transformations,transformed_matrices) = List.split @@ List.map (transform_linearly_matrix) matrices in
  (* the next 3 lines compute (eta^-1 @@ update @@ eta)(x) as stated in Termination of Polynomial Loops Thm. 5*)
  let new_update = List.concat @@ List.map2 matrix_times_vector transformations blocks (* eta(x) *)
                        |> apply_poly_transformation (*(update(eta(x))*)
                            concat_blocks
                            (List.map (fun x -> RealPolynomial.of_intpoly @@ Option.get (TransitionLabel.update transition x))
                                      concat_blocks
                            )
                        |> apply_poly_transformation (*eta^(-1)(update(eta(x)))*)
                            concat_blocks
                            (List.concat @@ List.map2 matrix_times_vector (List.map (Lacaml.D.Mat.transpose_copy) transformations) blocks)
                        |> List.map RealPolynomial.to_intpoly in (*cast it back to int_poly *)
  List.iter (print_string) (List.map Polynomial.to_string (List.map (fun x -> Option.get (TransitionLabel.update transition x)) concat_blocks));  (* Print update of transition *)
  Printf.printf "\n506: \n "; List.iter (print_string) (List.map (fun x -> "; " ^ Polynomial.to_string_pretty x) new_update);

  (* this approach was based on the wrong assumption that only linear parts had to be transformed
  let linear_parts = List.map RealPolynomial.to_intpoly @@ List.concat @@ List.map2 matrix_times_vector (List.map2 (fun x y -> Lacaml.D.Mat.sub x y) transformed_matrices matrices) blocks in (* compiler needs the lambda function *)
  let update1 = List.map2 Polynomial.add (List.map (fun x -> Option.get (TransitionLabel.update transition x)) (List.concat blocks)) (linear_parts) in
  *)
  

  let guard = TransitionLabel.guard_without_inv transition in 
  let guard_as_atom_list = Constraints.Constraint.atom_list guard in 
  let guard_polys = List.map RealPolynomial.of_intpoly @@ List.map Atom.poly guard_as_atom_list in 
  Printf.printf "\n 611: %s\n " @@ RealPolynomial.to_string (RealPolynomial.mult_with_const (OurFloat.of_float 3.3) (List.first guard_polys)); 
  (* let guard_comps = List.map Atom.compkind updated_guard TODO *)
  let updated_guard_polys = apply_poly_transformation concat_blocks (List.concat (List.map2 matrix_times_vector transformations blocks)) guard_polys in 
  let updated_guard_atoms = List.map (fun poly -> RealAtom.mk RealAtom.Comparator.LT poly RealPolynomial.zero) updated_guard_polys in (*TODO stattdessen map_polynomial benutzen *)
  let updated_guard = Constraints.RealConstraint.mk updated_guard_atoms in 

  (*
                            ~guard:(TransitionLabel.guard_without_inv transition)  *)
  let updated_transition = TransitionLabel.mk 
                            ~cost:(TransitionLabel.cost transition) 
                            ~guard:(TransitionLabel.guard_without_inv transition) (*TODO updated_guard*) 
                            ~assignments:new_update 
                            ~patterns:concat_blocks 
                            ~vars:(VarSet.to_list (TransitionLabel.vars transition)) 
                          |> (flip TransitionLabel.add_invariant) (TransitionLabel.invariant transition) in


   (* unbound value means not in mli 
*)

 proof := FormattedString.Empty;
  let opt = TimeBoundTable.find_option time_bound_table (l,transition,l') in
  if Option.is_none opt then (
    print_string "\n 396 TWN";
    let bound =
      Timeout.timed_run 5. (fun () -> try
        let parallel_edges = parallel_edges [] (TransitionSet.to_list scc) in
        let cycle = find_cycle appr program ( (*find_cycle throws an exception if no cycle is found (for efficiency reasons) *)
          if Location.equal l l' then
            let f (l1,loop,l1') = Location.equal l l1 && Location.equal l' l1' && String.equal (TransitionLabel.update_to_string_rhs transition) (TWNLoop.update_to_string_rhs loop) in
            [[List.find f parallel_edges]]
          else
            (cycles (parallel_edges |> List.filter (fun (l,_,l') -> not (Location.equal l l')) |> Set.of_list) l ([([(l,(TWNLoop.mk_transition transition),l')], (LocationSet.singleton l'))]) []))
        in
        print_string "\n 397 TWN";
        print_int (List.length cycle);
        print_string (List.fold_left (^) "" (List.map (fun x -> "a") cycle));
        let handled_transitions = ListMonad.(cycle >>= fun (l,twn,l') -> TWNLoop.subsumed_transitions l l' twn) in
        let entries = Program.entry_transitions logger program handled_transitions in
        (* add_to_proof_graph program handled_transitions entries; *)
        Logger.log logger Logger.INFO (fun () -> "cycle", ["decreasing", Transition.to_id_string (l,transition,l'); "cycle", (TransitionSet.to_id_string (TransitionSet.of_list handled_transitions)); "entry", (TransitionSet.to_id_string (TransitionSet.of_list entries))]);
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
      (print_string "\n 425 TWN";
      bound |> Option.get |> Tuple2.first |> tap (fun b -> Logger.log logger Logger.INFO (fun () -> "twn", ["global_bound", Bound.to_string b]))
      |> tap (fun _ -> proof_append FormattedString.((mk_str_line (bound |> Option.get |> Tuple2.first |> Bound.to_string ~pretty:true)))))
    else (
      print_string "\n 429 TWN";
      Logger.log logger Logger.INFO (fun () -> "twn", ["Timeout", Bound.to_string Bound.infinity]);
      Bound.infinity)
  )
  else (
    print_string "\n 434 TWN";
    let cycle, xs = Option.get opt in
    let bound_with_sizebound = Bound.sum_list (List.map (fun (entry, bound) -> lift appr entry bound) xs) in
    bound_with_sizebound |> tap (fun b -> Logger.log logger Logger.INFO (fun () -> "twn", ["global_bound", Bound.to_string b]))
                         |> tap (fun b -> proof_append FormattedString.((mk_str_line (b |> Bound.to_string ~pretty:true))))))
  |> tap (fun b -> if Bound.compare_asy b (Approximation.timebound appr (l,transition,l')) < 0 then ProofOutput.add_to_proof @@ fun () ->
    FormattedString.((mk_header_big @@ mk_str "Time-Bound by TWN-Loops:") <>
                      mk_header_small (mk_str ("TWN-Loops: t" ^ (TransitionLabel.id transition |> Util.natural_to_subscript) ^ " " ^ Bound.to_string ~pretty:true b)) <>
                      !proof))
