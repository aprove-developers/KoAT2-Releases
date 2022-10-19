open Batteries
open Polynomials
open ProgramModules
open Formulas
open Atoms
open BoundsInst
open Constraints
open PolyExponential
open TransitionLabel
open Automorphism

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
    Option.some blocks
  else
    None

let check_solvable_t (t: TransitionLabel.t) = check_solvable (TWNLoop.mk_transition t)

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
  if (n == 0) then []
  else
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
module SMTSolverTimeout = SMT.Z3SolverTimeout

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

let check_update_invariant twn_loop atom =
  let poly = Atom.poly atom in
  let poly_updated = Polynomial.substitute_f (TWNLoop.update_full twn_loop) poly in
  let atom_updated = Atom.mk_lt poly_updated Polynomial.zero in
  SMTSolver.tautology Formula.(implies (mk [atom]) (mk [atom_updated]))

let termination_ twn order npe varmap =
  let self_impl, rest = TWNLoop.invariant twn |> List.partition @@ check_update_invariant twn in
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
 |> tap (fun bool ->
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
module ParameterMonomial = Monomials.Make(PolynomialOver(OurInt))

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
    |> tap (fun b -> (* Logger.log logger Logger.INFO (fun () -> "complexity.compute_f", ["2*alpha_abs+max(N,M)", Bound.to_string b]);  *)
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

let check_twn (_,t,_) =
 check_twn (TWNLoop.mk_transition t)

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

(* Computes all cycles containing l0. The function call "cycles trans l0 [l0 ->_t l1] []" returns all paths containing t *)
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
  List.fold TWNLoop.append t1 (post@pre)

(** get_linear_update_of_variable (x<- 2x+3y+y^2) x y returns 3 *)
let get_linear_update_of_variable (t:TWNLoop.t) (var_left:TWNLoop.VarMap.key) (var_right:TWNLoop.VarMap.key)=
  match (TWNLoop.update t var_left) with
    | None -> (OurInt.of_int 0)
    | Some update ->
  let fupdate = List.filter (fun (x,y) -> Monomial.is_univariate_linear y) (Polynomial.monomials_with_coeffs update) in
  let (x,y) = List.find  (fun (x,y) -> (Var.equal (List.first (VarSet.to_list (Monomial.vars y))))  var_right) fupdate in
  x

(** get_linear_update_list [(x<- 2x+3y+y^2) x [x;y]] returns [[2;3]] *)
let rec get_linear_update_list (t:TWNLoop.t) (var_left:TWNLoop.VarMap.key) (block:TWNLoop.VarMap.key list) = match block with
  | [] -> []
  | x::xs -> get_linear_update_of_variable t var_left x :: get_linear_update_list t var_left xs

let matrix_of_linear_assignments (t:TWNLoop.t) (block:TWNLoop.VarMap.key list) =
  List.map (fun x -> get_linear_update_list t x block) block


(** [matrix_times_vector A x] returns a polynomial list where each element stores a row of [A*x] *)
let matrix_times_vector_rational (matrix:OurRational.t list list) (vars:TWNLoop.VarMap.key list) =
  List.map (fun xs -> RationalPolynomial.of_coeff_list xs vars) matrix

let matrix_times_vector_int (matrix:OurInt.t list list) (vars:TWNLoop.VarMap.key list) =
  List.map (fun xs -> Polynomial.of_coeff_list xs vars) matrix

(** sorts the blocks from function check_solvable in the order defined in the transition (needs O(n^2 log n) due to index_of) *)
let change_order t blocks =
  let var_list = (VarSet.to_list (TWNLoop.vars t)) in
  let blocks = List.map (List.sort (fun x y ->
                                            if List.index_of x var_list < List.index_of y var_list then -1 else 1)
                        ) blocks in
  List.sort (fun x y -> if List.index_of (List.first x) var_list < List.index_of (List.first y) var_list then -1 else 1) blocks

let read_process_lines command = (* This function was written by Tom Küspert *)
  let lines = ref [] in
  let in_channel = Unix.open_process_in command in
  begin
    try
      while true do
        lines := input_line in_channel :: !lines
      done;
    with
      | BatInnerIO.Input_closed -> ()
      | End_of_file -> ()
  end;
  List.rev !lines

(** returns python-friendly list (not list of list) of [matrix]*)
let matrix_to_string matrix =
  let flattened_matrix = List.flatten matrix in
  "["
  ^ List.fold (fun x y -> x  ^ (OurInt.to_string y)^ ",") "" (List.take (List.length flattened_matrix -1) flattened_matrix)
  ^ (OurInt.to_string (List.last flattened_matrix))
  ^ "]"

(** separates [xs] into several list of length n. If [n] does not divide the length of the list, the last chunk will be shorter*)
let rec list_lift (n:int) (xs:'a list) : 'a list list = match  xs with
    | [] -> []
    | xs -> let (a,b) = List.takedrop n xs in a::(list_lift n b)


(** turns string into OurInt list list (which represents a matrix).
  [parse_matrix d s] return the matrix of dimension [d] times [d] that is described with string [s] *)
let parse_int_matrix (dim:int) (s:string) =
  Str.split (Str.regexp "[^0-9/./\\-]+") s (*turn string into list, splitted at each non number char *)
      |> List.map int_of_string
      |> List.map OurInt.of_int
      |> list_lift dim

(** [parse_matrix d s] return the matrix of dimension [d] times [d] that is described with string [s] *)
let parse_matrix (dim:int) (s:string) =
  Str.split (Str.regexp "[^0-9/./\\-]+") s (*turn string into list, splitted at each non number character *)
      |> List.map OurRational.of_string
      |> list_lift dim  (*turn OurRational list into OurRational list list *)


(**[transform_linearly_matrix A] returns the jordan decomposition [(T * B *T^{-1})] of [A] if all eigenvalues are integer and
Here, [T^{-1}] is normalized, i.e., an integer matrix.
[A] has to be square *)
let transform_linearly_matrix (matrix: OurInt.t list list) =
  if List.length matrix == 1 (* nothing to transform *)
      then Some ( [[(OurInt.of_int 1, OurInt.of_int 1)]], matrix,  [[OurInt.of_int 1]])
  else
    let command = "python3 -c 'from src.bounds.JordanNormalForm import jordan_normal_form; jordan_normal_form(" ^ matrix_to_string matrix ^ ")'" in
    (* the python output consits of 3 matrices: T, J, T^-1 (which is normalized, see jordan normal form) or gives an error string *)
    let python_output = read_process_lines command in
    match python_output with
      | [a;b;c] ->    Some(parse_matrix (List.length matrix) a,
                        	 parse_int_matrix (List.length matrix) b,
                           parse_int_matrix (List.length matrix) c)
      | _ -> None (*error string *)

(**[transform_with_aut twn_loop automorphism vars] transforms a loop by the given automorphism,
i.e., applies the inverse to the invariant and guard and transforms the update   *)
let transform_with_aut twn_loop automorphism vars =
  match VarMap.map RationalPolynomial.of_intpoly @@ TWNLoop.update_map twn_loop (*current update *)
                  |> Automorphism.transform_update automorphism with
                  |  None -> None
                  |  Some x -> let  new_update = List.map Polynomial.simplify x in
  let updated_guard =  TWNLoop.Guard.atoms @@ TWNLoop.guard_without_inv twn_loop (*current guard *)
                    |> Automorphism.transform_guard automorphism in
  let updated_invariant = Automorphism.transform_guard automorphism @@ TWNLoop.invariant twn_loop in
  let new_transitionlabels = TWNLoop.subsumed_transitionlabels twn_loop
                             |> List.map (fun t -> TransitionLabel.mk
                            ~id:(TransitionLabel.id t |> Option.some)
                            ~cost:(TransitionLabel.cost TransitionLabel.default) (*TODO sind die kosten von bedeutung? *)
                            ~guard:updated_guard
                            ~assignments:new_update
                            ~patterns:vars
                            ~vars:(VarSet.to_list (TWNLoop.vars twn_loop))) in
  (*return updated transition and automorphism*)
  Some (TWNLoop.mk_transitions new_transitionlabels
        |> (flip TWNLoop.add_invariant) updated_invariant )

(** [transform_linearly loop transformation_type] first checks whether a loop is in twn-form.
    Then tries to find a  transformation using the jordan decomposition. To compute it, we call sympy.*)
let transform_linearly (transition: TWNLoop.t) transformation_type =
  (* find order for variables and independent blocks and sort them for elegant code when updating transition*)
  match (check_solvable transition) with
    | None -> None
    | Some x -> let blocks = change_order transition x in
  if List.length blocks == List.length (VarSet.to_list (TWNLoop.vars transition)) then
      Some (transition, Automorphism.identity_aut) (*loop already is in twn form*)
  else if transformation_type != `TWNTransformJordan  && transformation_type != `TWNTransform  then
    None
  else
    let concat_blocks = List.concat blocks in
    (* compute linear update matrices *)
    let matrices = List.map (matrix_of_linear_assignments transition) blocks in
    let transformations = List.map (transform_linearly_matrix) matrices in (*(transformations,transformed_matrices) *)
    if not @@ List.for_all Option.is_some transformations then
      None
    else
      let (transformations,js,transformation_invs) =
        List.fold_right (fun  (x1,x2,x3) (xs1,xs2,xs3)-> ((x1::xs1) , (x2::xs2) ,(x3::xs3)))
                  (List.map Option.get transformations)
                  ([],[],[]) in
      let eta = List.concat @@ List.map2 matrix_times_vector_rational transformations blocks in
      let eta_inv = List.concat @@ List.map2 matrix_times_vector_int transformation_invs blocks in
      let automorphism = Automorphism.of_poly_list concat_blocks eta eta_inv in
      match transform_with_aut transition automorphism concat_blocks with
      | None -> None
      | Some x -> Some(x, automorphism)

(** transform_non_linearly transforms a TWNLoop into twn form, it starts with the degree 1 and then counts upwards,
    until for some degree the formulas get too large. The degree of the inverse is the same as the degree of the automorphism
*)
let rec transform_non_linearly ?(degree =1) (t: TWNLoop.t) =
  let vars = VarSet.to_list @@ TWNLoop.vars t in
  try let endomorphism = Endomorphism.of_degree vars degree in
      let inv_formula = Endomorphism.formula_to_check_invertibility endomorphism in
      let twn_formula = Endomorphism.formula_to_check_twn vars endomorphism (TWNLoop.update_map t) in
      match SMTSolverTimeout.get_model @@ Formula.simplify @@ Formula.mk_and inv_formula twn_formula with
        | None ->  transform_non_linearly ~degree:(degree +1) t
        | Some valuation -> let automorphism = Automorphism.of_endomorphism endomorphism valuation in
          match transform_with_aut t automorphism vars with
          | None -> None (* transform with aut returns None iff result is not integer update, however this should never be the case as here we use integer automorphisms  *)
          | Some x -> Some (x, automorphism)
  with | Stack_overflow -> None

(** transform tries to transform a given loop into twn-form. First checks whether the given loop is twn;
then tries the Jordan approach;
then tries general approach
Returns the resulting twn-loop and an transformation automorphism *)
let transform transformation_type ((entry, x, t): Transition.t * (Transition.t list * Transition.t list) * TWNLoop.t)  =
   match (transform_linearly t transformation_type) with
  | Some (transformed, automorphism) ->  Some (entry, x, transformed, automorphism)
  | None -> if transformation_type ==  `TWNTransformGeneral || transformation_type ==  `TWNTransform then
              match (transform_non_linearly t) with
                    | Some (transformed, automorphism) -> Some (entry, x, transformed, automorphism)
                    | None -> None
            else
              None

let corresponding_transition t (trans: TransitionSet.t) = TransitionSet.find_first (fun (_,t',_) -> (TransitionLabel.id t) == (TransitionLabel.id t')) trans

(* Finds entered location on cycle. *)
let rec find l scc list =
    match list with
    | [] ->  raise Not_found
    | t::xs -> let (_,_,l') = corresponding_transition t scc in if Location.equal l l' then 0 else 1 + (find l scc xs)

(** lift_option [[Some x; Some y]] returns [Some [x;y]]. Returns none if none is element of list. *)
let lift_option (xs:'a option list) : 'a list option =
  if List.for_all Option.is_some xs then
    Some (List.map Option.get xs)
  else
    None

exception No_Cycle

let check_non_increasing twn_loop t =
  List.for_all (fun atom ->
    (** Here, an atom has the form poly < 0. *)
    let poly = Atom.poly atom in
    let poly_updated = Polynomial.substitute_f (TransitionLabel.update_full t) poly in
    let atom = Atom.mk_le poly poly_updated |> Atom.neg
    and guard = TransitionLabel.guard t in
    SMTSolver.tautology Formula.(implies (mk guard) (mk [atom]))) (twn_loop |> TWNLoop.guard_without_inv |> Formula.atoms |> List.filter (check_update_invariant twn_loop))

let find_non_increasing_set program appr decreasing loop entry =
  let rec f non_increasing entries =
    if List.for_all (fun entry -> (Approximation.is_time_bounded appr entry) && (Approximation.is_size_bounded program appr entry)) entries then
      Option.some (non_increasing, entries)
    else
      let bounded, unbounded = List.partition (fun entry -> (Approximation.is_time_bounded appr entry) && (Approximation.is_size_bounded program appr entry)) entries in
      if List.for_all (check_non_increasing loop) (List.map Tuple3.second unbounded) then
        let new_entries =
          bounded @ (Program.entry_transitions logger program (unbounded @ non_increasing))
        in
        f (unbounded @ non_increasing) new_entries
      else
        None in
  f (List.map (fun t -> corresponding_transition t (Program.transitions program)) decreasing) [entry]

let find_cycle appr scc program (cycles: path list) transformation_type =
  try
  List.find_map (fun cycle ->
    (* list of all transitions in a cycle, twnloop doesn't require each transition to be in twn form, but together they do *)
      let handled_transitions = List.fold (fun xs (l,twn,l') -> (List.map (fun t -> (l,t,l')) (TWNLoop.subsumed_transitionlabels twn))@xs) [] cycle in
      let entries = Program.entry_transitions logger program handled_transitions in
      let twn_loops = List.map (fun (_,_,l') -> compose_transitions (cycle |> List.map Tuple3.second) (find l' scc (cycle |> List.map (List.first % TWNLoop.subsumed_transitionlabels % Tuple3.second)))) entries in (* 'find' throws an exception *)
      let loop_entry = (List.combine twn_loops entries) in
      if (List.for_all (fun (t, entry) ->
          let eliminated_t = (* throw out useless variables *)
            EliminateNonContributors.eliminate_t
              (TWNLoop.input_vars t) (TWNLoop.Guard.vars @@ TWNLoop.guard t) (TWNLoop.update t) (TWNLoop.remove_non_contributors t)
          in
          not (VarSet.is_empty (TWNLoop.vars eliminated_t)) (* Are there any variables *)
          && VarSet.equal (TWNLoop.vars eliminated_t) (TWNLoop.input_vars eliminated_t) (* No Temp Vars? *))
        loop_entry
      ) then (
        let subsumed_transitionlabels = List.map (TWNLoop.subsumed_transitionlabels % Tuple3.second) cycle |> List.fold (@) [] in
        let non_increasing_opt = List.map (uncurry @@ (find_non_increasing_set program appr subsumed_transitionlabels)) loop_entry in
        if List.for_all Option.is_some non_increasing_opt then (
          lift_option @@ List.map (transform transformation_type) (List.combine (List.map Option.get non_increasing_opt) twn_loops |> List.combine entries |> List.map (fun (x,(y,z)) -> (x,y,z)))) (*tries to transform into loops. *)
        else
          None)
      else
        None  (*Not twn nor twn-transformable *)
    )
    cycles
    with
    | Not_found -> raise No_Cycle

(** Gets a list of transitions and rec. merges them into twn-loops, i.e., disjunctions of transitions. *)
let rec parallel_edges ys = function
  | [] -> ys
  | (l,t,l')::xs -> let f (l1,loop,l1') = Location.equal l l1 && Location.equal l' l1' && String.equal (TransitionLabel.update_to_string_rhs t) (TWNLoop.update_to_string_rhs loop) in
                    let h (l1,t1,l1') = Location.equal l l1 && Location.equal l' l1' && String.equal (TransitionLabel.update_to_string_rhs t) (TransitionLabel.update_to_string_rhs t1) in
  if List.exists f ys then parallel_edges ys xs
  else parallel_edges ((l, TWNLoop.mk_transitions List.(map Tuple3.second (filter h ((l,t,l')::xs))), l')::ys) xs

module TimeBoundTable = Hashtbl.Make(Transition)

(* keys: liste von transition (kreise), values: bounds von eingangstransitionen *)
let time_bound_table: ((Transition.t list) * (Transition.t list * Bound.t) list) TimeBoundTable.t = TimeBoundTable.create 10

let lift appr entries bound =
  List.map (fun entry ->
  let bound_with_sizebound = Bound.substitute_f (Approximation.sizebound appr entry) bound in
    Bound.mul (Approximation.timebound appr entry) bound_with_sizebound
    |> tap (fun b ->
    proof_append (FormattedString.(mk_paragraph (mk_str_line ("relevant size-bounds w.r.t. t" ^ (Transition.id entry |> Util.natural_to_subscript) ^ ":") <> (
          Bound.vars bound
          |> VarSet.to_list
          |> List.map (fun v -> (Var.to_string ~pretty:true v) ^ ": " ^ (Approximation.sizebound appr entry v |> Bound.to_string ~pretty:true))
          |> List.map (FormattedString.mk_str_line) |> FormattedString.mappend) <>
          FormattedString.mk_str_line ("Runtime-bound of t" ^ (Transition.id entry |> Util.natural_to_subscript) ^ ": " ^ (Approximation.timebound appr entry |> Bound.to_string ~pretty:true)) <>
          FormattedString.mk_str ("Results in: " ^ (Bound.to_string ~pretty:true b))))))) entries
  |> Bound.sum_list

let check_update_invariant twn_loop atom =
  let poly = Atom.poly atom in
  let poly_updated = Polynomial.substitute_f (TWNLoop.update_full twn_loop) poly in
  let atom_updated = Atom.mk_le poly_updated Polynomial.zero in
  SMTSolver.tautology Formula.(implies (mk [atom]) (mk [atom_updated]))

let time_bound (l,transition,l') scc program appr transformation_type =
  proof := FormattedString.Empty;
  let opt = TimeBoundTable.find_option time_bound_table (l,transition,l') in
  let bound =
  if Option.is_none opt then (
    let bound =
      Timeout.timed_run 5. (fun () -> try
        let parallel_edges = parallel_edges [] (TransitionSet.to_list scc) in
        let (entries_org, non_incr_entries, cycle, automorphisms) =
          let cycles = find_cycle appr scc program ( (* find_cycle throws an exception if no cycle is found (for efficiency reasons) *)
            if Location.equal l l' then
              let f (l1,loop,l1') = Location.equal l l1 && Location.equal l' l1' && String.equal (TransitionLabel.update_to_string_rhs transition) (TWNLoop.update_to_string_rhs loop) in
              [[List.find f parallel_edges]]
            else
              (cycles (parallel_edges |> List.filter (fun (l,_,l') -> not (Location.equal l l')) |> Set.of_list) l ([([(l,(TWNLoop.mk_transition transition),l')], (LocationSet.singleton l'))]) [])) transformation_type in
          List.fold_right (fun (x0,x1,x2,x3) (xs0,xs1,xs2,xs3) -> ((x0::xs0),(x1::xs1),(x2::xs2),(x3::xs3))) cycles ([],[],[],[])
        in
        let handled_transitions =
          ListMonad.(cycle >>= fun loop -> TWNLoop.subsumed_transitions scc loop)
        in
        let twn_loops =
          List.map (fun (l,t,l') -> compose_transitions cycle (find l' scc (cycle |> List.map (List.first % TWNLoop.subsumed_transitionlabels)))) entries_org
        in
        proof_append FormattedString.((mk_header_small (mk_str "Cycles:")) <>
          (List.combine (List.map Transition.to_string_pretty entries_org) (List.map (TWNLoop.to_string ~pretty:true) twn_loops)
          |> List.map (fun (a,b) -> FormattedString.mk_str_line ("entry: " ^ a) <> FormattedString.mk_block (FormattedString.mk_str_line ("results in twn-loop: " ^ b)))
          |> FormattedString.mappend));
        let global_local_bounds =
          List.map (fun (entry_org, (non_increasing,new_entries), loop, automorphism) -> (* entry,twn,automorphism *)
            proof_append FormattedString.(mk_header_small (mk_str @@ "Cycle by " ^  (Transition.to_id_string_pretty entry_org)));
            let program_with_one_entry =
              let non_entries =
                TransitionSet.to_list (TransitionSet.diff (Program.transitions program) (TransitionSet.of_list entries_org))
              in
              Program.from (
                non_entries (* TODO simplify *)
                |> List.map List.singleton
                |> (@) [[entry_org]])
                (Program.start program)
            in
            let twn_inv =
              program_with_one_entry
              |> InvariantGeneration.transform_program
              |> MaybeChanged.unpack
              |> Program.transitions
              |> TransitionSet.filter (fun t -> List.exists (Transition.equal t) handled_transitions)
              |> TransitionSet.to_list
              |> List.map (TransitionLabel.invariant % Transition.label)
              |> fun invariants -> List.flatten invariants |> List.filter (fun atom -> List.for_all (List.exists (Atom.equal atom)) invariants)
              |> TWNLoop.add_invariant loop in
            let eliminated_t = EliminateNonContributors.eliminate_t
                (TWNLoop.input_vars twn_inv) (TWNLoop.Guard.vars @@ TWNLoop.guard twn_inv) (TWNLoop.update twn_inv) (TWNLoop.remove_non_contributors twn_inv)
            in
            if VarSet.is_empty (TWNLoop.vars eliminated_t) then
              Bound.infinity, ([entry_org], Bound.infinity)
            else (
              let bound = Automorphism.transform_bound automorphism @@ complexity eliminated_t in
              if Bound.is_infinity bound then
                raise (Non_Terminating (handled_transitions, [entry_org]));
                proof_append FormattedString.(
                  mk_header_small (mk_str @@ "Lift Bound: ")
                  <> mk_str_line @@ "Compute new entries: " ^ (new_entries |> List.enum |> Util.enum_to_string Transition.to_id_string_pretty)
                  <> mk_str_line @@ "Non increasing transitions: " ^ (non_increasing |> List.enum |> Util.enum_to_string Transition.to_id_string_pretty));
                lift appr new_entries bound, (new_entries, bound)))
          (List.map2 (fun (a,b) (c,d) -> (a,b,c,d)) (List.combine entries_org non_incr_entries) (List.combine twn_loops automorphisms))
        in
        List.iter (fun t -> TimeBoundTable.add time_bound_table t (handled_transitions, List.map Tuple2.second global_local_bounds)) handled_transitions;
        global_local_bounds |> List.map Tuple2.first |> Bound.sum_list
        with
        | No_Cycle -> Logger.log logger Logger.DEBUG (fun () -> "twn", ["no twn_cycle found", ""]); Bound.infinity
        | Non_Terminating (handled_transitions,entries)->
            Logger.log logger Logger.DEBUG (fun () -> "twn", ["non terminating", ""]);
            List.iter (fun t -> TimeBoundTable.add time_bound_table t (handled_transitions, List.map (fun t -> ([t], Bound.infinity)) entries)) handled_transitions;
            Bound.infinity)
    in
    if Option.is_some bound then
      bound |> Option.get |> Tuple2.first
    else (
      Logger.log logger Logger.INFO (fun () -> "twn", ["Timeout", Bound.to_string Bound.infinity]);
      Bound.infinity)
  )
  else (
    let cycle, xs = Option.get opt in
    let bound_with_sizebound = Bound.sum_list (List.map (fun (entry, bound) -> lift appr entry bound) xs) in
    bound_with_sizebound |> tap (fun b -> Logger.log logger Logger.INFO (fun () -> "twn", ["global_bound", Bound.to_string b]))
                         |> tap (fun b -> proof_append FormattedString.((mk_str_line (b |> Bound.to_string ~pretty:true))))) in
    (if Bound.compare_asy bound (Approximation.timebound appr (l,transition,l')) < 0 then
      let formatted = FormattedString.((mk_header_big @@ mk_str "Time-Bound by TWN-Loops:") <>
              mk_header_small (mk_str ("TWN-Loop t" ^ (TransitionLabel.id transition |> Util.natural_to_subscript) ^ " with runtime bound " ^ Bound.to_string ~pretty:true bound)) <>
              !proof) in
      ProofOutput.add_to_proof @@ fun () -> formatted);
    bound
