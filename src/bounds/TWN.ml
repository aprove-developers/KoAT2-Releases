open Batteries
open Polynomials
open ProgramTypes
open Formulas
open Atoms
open BoundsInst
open Constraints
open PolyExponential
open TransitionLabel
open Automorphism

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

(** get_linear_update_of_variable (x<- 2x+3y+y^2) x y returns 3 *)
let get_linear_update_of_variable (t:TWNLoop.t) (var_left:TWNLoop.VarMap.key) (var_right:TWNLoop.VarMap.key)=
  let update = Option.get (TWNLoop.update t var_left) in
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


(*
let pairwise_distinct xs =
  let sorted_xs = List.sort (fun x y -> if x<y then 1 else -1) xs in 
  let rec pairwise_distinct_help = function 
    x::y::xs -> if x == y then false else pairwise_distinct_help (y::xs)
   |  _ -> true  
  in pairwise_distinct_help sorted_xs  *)

let list_list_to_string xss =
  let yss = List.map (fun xs -> List.map (fun x -> OurInt.to_string x) xs) xss in
  let zs = List.map (fun ys -> List.fold (fun x y -> x ^y^"; ") " " ys) yss in
  List.fold (fun x y -> x^y^"; " ) " " zs

  
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
  Str.split (Str.regexp "[^0-9/./\-]+") s (*turn string into list, splitted at each non number char *)
      |> List.map int_of_string (* turn string list into float list *)
      |> List.map OurInt.of_int
      |> list_lift dim 

(** turns python string "a/b"into OurRational a/b  *)
let string_to_our_rational (s:string) = 
  let fraction = Str.split (Str.regexp "[^0-9.\-]+") s in 
  if List.length fraction == 1 then 
    OurRational.of_int @@ int_of_string @@ List.first fraction
  else 
    OurRational.of_int_tuple (int_of_string @@  List.at fraction 0, int_of_string @@  List.at fraction 1)

(** turns string into OurRational list list (which represents a matrix).
  [parse_matrix d s] return the matrix of dimension [d] times [d] that is described with string [s] *)
let parse_matrix (dim:int) (s:string) = 
  Str.split (Str.regexp "[^0-9/./\-]+") s (*turn string into list, splitted at each non number char *)
      |> List.map string_to_our_rational  (* turn string list into float list *)
      |> list_lift dim  (*turn rational list into rational list list *)
      

(* matrix is square *)
let transform_linearly_matrix (matrix: OurInt.t list list) = 
  if List.length matrix == 1 (* nothing to transform *)
      then Some ( [[(OurInt.of_int 1, OurInt.of_int 1)]], matrix,  [[OurInt.of_int 1]]) (*Todo as 2. argument return matrix *)
      (*Some (Lacaml.D.Mat.of_list [[1.]], matrix, Lacaml.D.Mat.of_list [[1.]], Lacaml.D.Mat.of_list [[1.]])*)
  else 
    let command = "python3 -c 'from src.bounds.JordanNormalForm import jordan_normal_form; jordan_normal_form(" ^ matrix_to_string matrix ^ ")'" in  
    (* the python output consits of 4 matrices: T, J, T^-1, T^-1_nomralized, see jordan normal form or gives an error string *)
    let python_output = read_process_lines command in 
    match python_output with 
      | [a;b;c] ->  (*let [t; j; t_inverse; t_inverse_normalized] = (List.map (parse_matrix (List.length matrix)) [a;b;c;d]) in *)
                      Some(parse_matrix (List.length matrix) a, 
                        	 parse_int_matrix (List.length matrix) b,
                           parse_int_matrix (List.length matrix) c)
      | _ -> None (*error string *)
    

let transform_with_aut transition automorphism vars = 
  let new_update = VarMap.map RationalPolynomial.of_intpoly @@ TWNLoop.update_map transition (*current update *)
                  |> Automorphism.transform_update automorphism  
                  |> List.map Polynomial.simplify in
  let updated_guard =  TWNLoop.Guard.atoms @@ TWNLoop.guard_without_inv transition (*current guard *)
                    |> Automorphism.transform_guard automorphism in 
  let updated_invariant = Automorphism.transform_guard automorphism @@ TWNLoop.invariant transition in 
  (*return updated transition and automorphism*)
  (TWNLoop.mk_transition @@TransitionLabel.mk 
                            ~cost:(TransitionLabel.cost TransitionLabel.default) (*TODO sind die kosten von bedeutung? *)
                            ~guard:updated_guard
                            ~assignments:new_update 
                            ~patterns:vars
                            ~vars:(VarSet.to_list (TWNLoop.vars transition)) 
                          |> (flip TWNLoop.add_invariant) updated_invariant )


let transform_linearly (transition: TWNLoop.t) =
None  
  (* find order for variables and independent blocks and sort them for elegant code when updating transition   
  match (check_solvable transition) with 
    | None -> None 
    | Some x -> let blocks = change_order transition x in 
  if List.length blocks == List.length (VarSet.to_list (TWNLoop.vars transition)) then
     let guard = TWNLoop.guard_without_inv transition in 
      Printf.printf "\n 501: %s \n " (Guard.to_string (TWNLoop.Guard.atoms guard));
      Some (transition, Automorphism.identity_aut) (*loop already is in twn form*)
  else
    let concat_blocks = List.concat blocks in 
    (* calculate matrices *)
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
      Some (transform_with_aut transition automorphism concat_blocks, automorphism)   *)

(** transform_non_linearly transform a TWNLoop into twn form, it starts with the degree 1 and then counts upwards, until for some degree the formulas get too large. *)
let rec transform_non_linearly ?(degree =1) (t: TWNLoop.t) = print_string "\n 366 TWN"; 
  let vars = VarSet.to_list @@ TWNLoop.vars t in 
  let endomorphism = Endomorphism.of_degree vars degree in 
  Printf.printf " \n 875: %s  \n" @@ Endomorphism.to_string @@  endomorphism; 
  let inv_formula = Endomorphism.formula_to_check_invertibility endomorphism in
  print_endline "endline1";
  try let twn_formula = Endomorphism.formula_to_check_twn vars endomorphism (TWNLoop.update_map t) in
  print_endline "endline2";
  match SMTSolver.get_model @@ Formula.simplify @@ Formula.mk_and inv_formula twn_formula with
    | None -> print_endline "endline None"; None (* transform_non_linearly ~degree:(degree +1) t*)
    | Some valuation ->print_endline "endline Some"; let automorphism = Automorphism.of_endomorphism endomorphism valuation in 
      Some (transform_with_aut t automorphism vars, automorphism)   
  with | Stack_overflow -> print_endline "endline overflow";None (*transform_non_linearly ~degree:(degree -1) t*)


let transform ((entry, t):ProgramTypes.Transition.t * TWNLoop.t) =
  Printf.printf "transforming into twn \n";
   match (transform_linearly t) with 
  | Some (transformed, automorphism) -> Some (entry, (Transition.target entry, transformed, Transition.target entry ), automorphism)(*TODO find target *)
  | None -> match (transform_non_linearly t) with 
          | Some (transformed, automorphism) -> Some (entry, (Transition.target entry, transformed, Transition.target entry ), automorphism)
          | None -> None 

let to_string arg =
  if Option.is_some arg then
    "solvable: " ^ (Util.enum_to_string (Util.enum_to_string Var.to_string) (Option.get arg |> List.map List.enum |> List.enum))
  else
    "not solvable" (* Just for Testing *)


(* Finds entered location on cycle. *)
let rec find l list =
    match list with
    | [] ->  raise Not_found
    | (l',_,_)::xs -> if Location.equal l l' then 0 else 1 + (find l xs)


let lift_option (xs:'a option list) : 'a list option = 
  if List.for_all Option.is_some xs then 
    Some (List.map Option.get xs)
  else 
    None 

let find_cycle appr program (cycles: path list) =
  List.find_map (fun cycle ->
    (* list of all transitions in a cycle, twnloop doesn't require each transition to be in twn form, but together they do *)
      let handled_transitions = List.fold (fun xs (l,twn,l') -> (List.map (fun t -> (l,t,l')) (TWNLoop.subsumed_transitionlabels twn))@xs) [] cycle in
      let entries = Program.entry_transitions logger program handled_transitions in
      let twn_loops = List.map (fun (_,_,l') -> compose_transitions cycle (find l' cycle)) entries in (* 'find' throws an exception *)
      let entries_twn_loops = (List.combine entries twn_loops) in 
      if (not @@ List.is_empty @@ List.filter (fun (entry, t) ->
          let eliminated_t = (* throw out useless variables *)
            EliminateNonContributors.eliminate_t
              (TWNLoop.input_vars t) (TWNLoop.Guard.vars @@ TWNLoop.guard t) (TWNLoop.update t) (TWNLoop.remove_non_contributors t)
          in
          (VarSet.is_empty (TWNLoop.vars eliminated_t)) (* Are there any variables *)) 
        entries_twn_loops
      ) then 
        (
        print_string "\n 665 1 TWN";
        None 
      )
      else if not (List.for_all (fun (entry, t) ->
          let eliminated_t = (* throw out useless variables *)
            EliminateNonContributors.eliminate_t
              (TWNLoop.input_vars t) (TWNLoop.Guard.vars @@ TWNLoop.guard t) (TWNLoop.update t) (TWNLoop.remove_non_contributors t)
          in
           (VarSet.equal (TWNLoop.vars eliminated_t) (TWNLoop.input_vars eliminated_t))) (* No Temp Vars? *)
        entries_twn_loops
      ) then 
        (
        print_string "\n 665 2TWN";
        None 
      )
      else if not (List.for_all (fun (entry, t) ->
          let eliminated_t = (* throw out useless variables *)
            EliminateNonContributors.eliminate_t
              (TWNLoop.input_vars t) (TWNLoop.Guard.vars @@ TWNLoop.guard t) (TWNLoop.update t) (TWNLoop.remove_non_contributors t)
          in
          ((Approximation.is_time_bounded appr) entry)) 
        entries_twn_loops
      ) then 
        (
          (*List.iter (fun (x,b) -> print_string ((Transition.to_string x ) ^ "; " ^ (TWNLoop.to_string b ) ^ ";; ")) entries_twn_loops ;*)
        print_string "\n 665 some entry is not timed bounded TWN \n ";
        None 
      )
      else if (List.for_all (fun (entry, t) ->
          let start_location = Transition.target entry in 
          let eliminated_t = (* throw out useless variables *)
            EliminateNonContributors.eliminate_t
              (TWNLoop.input_vars t) (TWNLoop.Guard.vars @@ TWNLoop.guard t) (TWNLoop.update t) (TWNLoop.remove_non_contributors t)
          in
          not (VarSet.is_empty (TWNLoop.vars eliminated_t)) (* Are there any variables *)
          && VarSet.equal (TWNLoop.vars eliminated_t) (TWNLoop.input_vars eliminated_t) (* No Temp Vars? *)
          && (Approximation.is_time_bounded appr) entry) 
        entries_twn_loops
      ) then (
        print_string "\n 665 transforming into TWN\n";
        lift_option @@ List.map transform entries_twn_loops)
         (*let res = lift_option @@ List.map transform (List.combine entries twn_loops) in 
         match res with 
         | None -> None 
         | Some res -> List.map (fun (x,y,z) -> (x,(l,y,l'),z)) res *)
      else 
        (
        print_string "\n 665 5TWN";
        None )
      
    )
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


let test_for_time vars degree = 
  let start = Unix.gettimeofday () in 
  print_endline "";
  let endomorphism = Endomorphism.of_degree vars degree in 
  Printf.printf "Execution time building endomorphism: %fs\n%!" (Unix.gettimeofday () -. start);
  Printf.printf " \n 875: %s  \n" @@ Endomorphism.to_string @@  endomorphism; 
  print_endline "";
  let start = Unix.gettimeofday () in 
  let formula = Formula.simplify @@  Endomorphism.formula_to_check_invertibility endomorphism in
  Printf.printf "Execution time for formula building: %fs\n%!" (Unix.gettimeofday () -. start);
  print_endline "";
  
  let start = Unix.gettimeofday () in 
  match SMTSolver.get_model formula with 
    | None -> degree 
    | Some valuation -> 
                    Printf.printf "Execution time for model: %fs\n%!" (Unix.gettimeofday () -. start);
                    print_endline "";
                    Printf.printf " \n 876: %s  \n" @@ Valuation.to_string @@  valuation; 
                    degree


let time_bound (l,transition,l') scc program appr = (
  Printf.printf " 682 : %s \n" @@ TransitionLabel.to_string transition;
  Printf.printf " 683 : %s \n" @@ to_string @@ check_solvable_t transition;
  Printf.printf " 684 : %s \n" @@ Location.to_string l;
  let vars = VarSet.to_list @@TransitionLabel.vars transition in 
  let vars = [Var.of_string "Arg_0"; Var.of_string "Arg_1"] in 
  let y1 = Polynomial.sub ( (Polynomial.of_power (Var.of_string "Arg_1") 1)) (((Polynomial.of_power (Var.of_string "Arg_1") 0))) in 
  let y2 = Polynomial.add (Polynomial.zero) (((Polynomial.of_power (Var.of_string "Arg_1") 2))) 
          |> flip Polynomial.sub ( (Polynomial.of_power (Var.of_string "Arg_0") 1)) in
  let y1 = Polynomial.sub ( (Polynomial.of_power (Var.of_string "Arg_0") 2)) (((Polynomial.of_power (Var.of_string "Arg_1") 1))) 
  |>Polynomial.add Polynomial.one
  |>Polynomial.add (Polynomial.of_power (Var.of_string "Arg_0") 1)
  |>Polynomial.add (Polynomial.of_power (Var.of_string "Arg_0") 1)
  in 
  let y2 = Polynomial.add (Polynomial.one) (((Polynomial.of_power (Var.of_string "Arg_0") 1))) in 
  let y3 = Polynomial.add ( (Polynomial.of_power (Var.of_string "Arg_2") 1)) ( (Polynomial.of_power (Var.of_string "Arg_0") 2)) in 
  let endo = Endomorphism.of_poly_list vars @@ List.map ParameterPolynomial.of_polynomial @@ [y1;y2] in 
  let valuation = SMTSolver.get_model @@ Endomorphism.formula_to_check_invertibility endo in
  let automorphism = Automorphism.of_endomorphism endo @@ Option.get valuation in 
  let vars = VarSet.to_list@@ TransitionLabel.vars transition in 
  let new_transition = transform_with_aut (TWNLoop.mk_transition transition) automorphism vars  in 
  Printf.printf " 686 : %s \n" @@ TWNLoop.to_string @@ (TWNLoop.mk_transition transition);
  Printf.printf " 687 : %s \n" @@ TWNLoop.to_string new_transition;
  (*Printf.printf " 685 : %s \n" @@ Automorphism.to_string automorphism;*)
  (*let formel = Formula.mk_eq RationalPolynomial.one @@ RationalPolynomial.of_coeff_list [OurRational.of_int_tuple (1,2)] [Var.of_string "x"] in 
  let y2 = Polynomial.add ( (Polynomial.of_power (Var.of_string "x") 1)) (((Polynomial.of_power (Var.of_string "y") 2))) in 
  let y3 = Polynomial.mul (y2) (Polynomial.of_var (Var.of_string "x")) in 
  let a = ParameterPolynomial.of_coeff_list [y2] [Var.of_string "x"] in 
  let b = ParameterPolynomial.of_coeff_list [y3] [Var.of_string "x"] in
  let c = ParameterPolynomial.of_coeff_list [(((Polynomial.of_power (Var.of_string "y") 2)))] [Var.of_string "x"] in 
  let d = ParameterPolynomial.of_coeff_list [Polynomial.of_var (Var.of_string "a")] [Var.of_string "x"] in 
  let (e,f) = List.split @@ ParameterPolynomial.monomials_with_coeffs d in  
  Printf.printf "861: %s  \n" (Polynomial.to_string (List.first e));
  Printf.printf "861: %s  \n" (ParameterPolynomial.to_string a);
  Printf.printf "861: %s  \n" (ParameterPolynomial.to_string b);
  Printf.printf "861: %s  \n" (ParameterPolynomial.to_string c);

  
  let endomorphism = Endomorphism.of_poly_list [Var.of_string "y"; Var.of_string "x"] [ParameterPolynomial.of_var @@ Var.of_string "x"; ParameterPolynomial.of_var @@ Var.of_string "y"] in 
  
  let formula = Endomorphism.formula_to_check_invertibility endomorphism in
  let res  = Option.get @@ SMTSolver.get_model formula in
  Printf.printf " \n 870: %s  \n" @@ Valuation.to_string @@  res; 
  
  let y2 = ParameterPolynomial.sub ( (ParameterPolynomial.of_power (Var.of_string "x") 2)) (((ParameterPolynomial.of_power (Var.of_string "y") 1))) in 
  let y3 = ParameterPolynomial.add ( (ParameterPolynomial.of_power (Var.of_string "x") 1)) (((ParameterPolynomial.one))) in 
  let endomorphism = Endomorphism.of_poly_list [Var.of_string "x"; Var.of_string "y"] [y2;y3] in 
  let formula = Endomorphism.formula_to_check_invertibility endomorphism in
  
  let valuation  = Option.get @@ SMTSolver.get_model formula in
  Printf.printf " \n 871: %s  \n" @@ Valuation.to_string @@  valuation; 
  let res = List.map (ParameterPolynomial.eval_coefficients (fun x -> Valuation.eval x valuation)) @@ Endomorphism.inv_poly_list endomorphism in 
  List.iter (fun x -> print_string @@ Polynomial.to_string x ^ "; ") res ;
 
  let vars = [Var.of_string "x"; Var.of_string "y"] in 
  let twn_formula = Endomorphism.formula_to_check_twn vars endomorphism (TransitionLabel.update_map transition) in
  let valuation  = Option.get @@ SMTSolver.get_model twn_formula in
  Printf.printf " \n 872: %s  \n" @@ Valuation.to_string @@  valuation; 
  Printf.printf " \n 873: %s  \n" @@ Formula.to_string twn_formula; 
  let a = test_for_time vars 1 in 
  let a = test_for_time vars 2 in 
  let a = test_for_time vars 3 in 
  let update = VarMap.map ParameterPolynomial.of_polynomial @@ TransitionLabel.update_map transition in 
  let transformed = 
  List.map (ParameterPolynomial.substitute_all update) (Endomorphism.inv_poly_list endomorphism) 
                |> List.map (ParameterPolynomial.substitute_all (Endomorphism.poly_map endomorphism))  in 
  let vars = List.of_enum @@ VarMap.keys update in 
  
  let var = List.first vars in 
  let vars = VarSet.of_list vars in 
  let res = List.first transformed in 
  let coeffs = ParameterPolynomial.monomials_with_coeffs @@ List.first transformed in 
  let zero_coeffs =  (* List.filter (fun (coeff,monom) -> VarSet.subset (Monomial.vars monom) vars || 
                                (VarSet.subset (VarSet.of_list [var]) (Monomial.vars monom) && Polynomial.equal (Polynomial.of_monomial monom) (Polynomial.of_var var))) *) 
                    List.filter (fun  (coeff,monom) -> VarSet.subset (ParameterMonomial.vars monom) vars 
                                                    || (ParameterMonomial.equal (monom) (ParameterMonomial.of_var var)
                                                    && VarSet.subset (VarSet.of_list [var]) (ParameterMonomial.vars monom))) coeffs 

                    |> List.map Tuple2.first in

  let formula =  List.fold (fun formula poly -> Formula.mk_and formula (Formula.mk_eq poly Polynomial.zero)) Formula.mk_true zero_coeffs in 
  *)

 (* let formula = Formula.mk_eq sum (Polynomial.of_var (Var.of_string "x")) in 
  let result = SMTSolver.get_model (Formula.mk_and (Formula.mk self_impl) formula |> Formula.simplify) in *)
  

  (* y2 = 3*x^2 +3 *)
  (* y3 = x +y ---- y4 = -2x + 4y;   y5 = -x + y;    y6 = -x - y 
  let y1 = Polynomial.add ( (Polynomial.of_power (Var.of_string "x") 1)) (((Polynomial.of_power (Var.of_string "y") 1))) in 
  let y2 = Polynomial.add ( (Polynomial.of_power (Var.of_string "x") 1)) (((Polynomial.of_power (Var.of_string "y") 2))) in 
  let varmap = VarMap.add (Var.of_string "x") y1 VarMap.empty |> VarMap.add (Var.of_string "y") y2 in 
  let res = Polynomial.substitute_all varmap y1  in 
  Printf.printf "742: %s \n" (Polynomial.to_string res);
  let y2 = Polynomial.mult_with_const (OurInt.OurInt_of_int 3) (Polynomial.add (Polynomial.of_power (Var.of_string "x") 2) (Polynomial.one)) in
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
  print_string (OurInt.string_of_OurInt (get_linear_update_of_variable t (Var.of_string "Arg_1") (Var.of_string "Arg_0")));
  let matrix = matrix_of_linear_assignments t (List.first (Option.get (check_solvable_t t))) in
  Printf.printf "475: %s\n" (list_list_to_string matrix);
  let var_left = List.first (List.first (Option.get (check_solvable_t t))) in
  let update1 = Option.get (TransitionLabel.update t var_left) in
  Printf.printf "475: %s\n" (list_list_to_string matrix);
  let update1 = Polynomial.add update1 update1 in
  let update1 = (Option.get (TransitionLabel.update t var_left)) in
  Printf.printf "484: %s\n" (Polynomial.to_string_pretty update1);
    prints:

    Printf.printf " : %s \n" ;
    print_string "552 \n";
    List.iter (List.iter (List.iter (fun x -> print_string ((OurRational.to_string x) ^ "; ")))) transformations;
    List.iter (List.iter (fun x -> print_string ((Var.to_string x) ^ "; "))) blocks; 

  
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
  Polynome sind hier immer BigInt Polynome? ja,
  wie sinnvoll ist jordan normalform? reicht nicht eine Transformation in eine obere Dreieckmatrix (Schursche Normalform, diagonal genau dann wenn möglich)

  Wenn die transformation nicht rational nicht, wie bei dem beispiel, wie soll ich dann den Guard anpassen?
  Wie ersetze ich nun die Transition?/wo findet das berechnen der closed form usw statt?

  wie mache ich code abschnitte kenntlich die ich kopiert habe?
  dieses über console aufrufen ist nicht wirklich schnell, hast Du Erfahrungen zu anderen Methoden? zB Pycaml (Vielleicht vorkompilen)
  es könnte sein, dass der guard rational ist, obwohl der Automophismus zB x1 auf sqrt(2) * x^1 abbildet wenn er zB x1^2 < 0 lautet, die überprüfung für diesen spezialfall 
    müsste ich aber auch in python machen, da ich in ocaml nicht mit algebraischen zahlen umgehen kann
  Du meintest die Transformation klappt auch wenn (nicht nur die Transformationsmatrix rational ist) sondern auch die Normalform an sich. Wie soll das gehen?

  Wenn die Eigenwerte nicht ganzzahlig sind, dann verwerde ich. Wenn dann noch die Transformationsmatrix nicht rational ist, dann verwerfe ich auch. Kann dieser Fall überhaupt eintreten, dass T nicht rational ist, aber die eigenwerte ganzzahlig?
  Man kann transitionen mit reellen polynomen konstruieren. Kann KoAT das analysieren? Denn reelle Guards gehen ja zB nicht ne
  "List.for_all (Approximation.is_time_bounded appr) entries" macht doch gar keinen Sinn oder? dann fragt man das jedes mal für alle ab
  ich weiß nicht, ob ich was falsch mache, aber sympy ist dermaßen langsam, dass es vielleicht sinnvoll ist, erst mit Lacaml zu überprüfen ob die eigenwerte int sind bevor man sympy benutzt

  eigenwerte berechnen kann sympy ziemlich schnell, es macht also viel sinn erstmal die zu berechnen, auf ganzzahligkeit zu überprüfen, und dann erst jordan 
  ich finde das highlighting von der doktorarbeit mit den examples sehr schön, weißt du zufällig wie man das macht? 
  Muss ich diese Schreibweise aus Lemma 9.1.7 übernehmen, oder kann ich sagen, dass polynome sowohl elemente des polynomringes sind als auch polynomfunktionen?
  Ist nicht die Startlocation gleich der endlocation in einem kreis? 
  Warum macht der die twn analyse nicht richtig? 

  nestedlinear wird nicht transformiert, weil obwohl sich die kreise ausschließen, das heißt der innere auf jeden fall linear ist und der äußere sorgt aber dafür dass die eine bedingung (entries are bounded) verletzt ist | enable die mprf
  Warum kommt splitUpTransformed timeout? aber das untransformierte nicht, es kommen nämlich guard bedingungen hinzu, wodurch die berechnungen anscheinend so viel schwerer werden, dass das ganze nicht terminiert in 5 sec| klappt mittlerweile
  splitUpStrange, da klappt das zusammenziehen irgendwie nicht, ist kein twn loop weil wohl ein entry nicht bounded ist ???
  So, ich denke ich muss mit nicht linearen Automorphismen anfangen, wie?
  Kolloqium: Zeit 25, PowerPoint jo, Live Demo jo mittwoch 16 uhr probevortrag
  docker jo 
  

  splitUp vs splitUp strange schafft das nicht, weil dem != statt dem > 

  Datenträger oder wie elektronisch einreichen? einfach email
  local  bound frage, ja einfach r
  wo sind die results 
  nimmt der alle testprogramme? 
  wo kann ich nur meine eigenen testen

  Der Smtsolver lucky guessed immer beim zweiten versuch, im ersten schafft er das nicht
  Stackoverflow beim konstruieren der formeln, beim zweiten einsetzen also in Endomorphis transform_update
  Der Timeout klappt nicht

  TODO 8.1.8 mit algorithmus abschreiben,cython (nur das starten geht schneller), beispiele, kreis

  parameter polynome, nach x umformen lassen, den koeffizienten= 1 setzen
  
  *)
  (*print_string "764";
  let vars = (VarSet.to_list (TransitionLabel.vars transition)) in 
  let y1 = Polynomial.add ( (Polynomial.of_power (List.at vars 1) 1)) (((Polynomial.of_power (List.at vars 2) 1))) in 
  let y2 = (Polynomial.of_power (List.at vars 2) 2) in
  let new_polynomials = [y1;y2] in 

  
  let invariant = TransitionLabel.invariant transition in 
  let updated_invariant = Guard.map_polynomial (compose_int_polynomials ([List.at vars 1;List.at vars 2]) new_polynomials) invariant in 
  let guard = TransitionLabel.guard_without_inv transition in 
  let uguard = Guard.map_polynomial (compose_int_polynomials ([List.at vars 1;List.at vars 2]) new_polynomials) guard in 
  Printf.printf "\n 720: %s " (Guard.to_string guard);
  Printf.printf "\n 721: %s " (Guard.to_string uguard);*)

  

   (* unbound value means not in mli 
*)
  
  proof := FormattedString.Empty;
  let opt = TimeBoundTable.find_option time_bound_table (l,transition,l') in
  if Option.is_none opt then (
    print_string "\n 396 TWN";
    let bound =
      Timeout.timed_run 5. (fun () -> try
        let parallel_edges = parallel_edges [] (TransitionSet.to_list scc) in 
        let (entries, cycle, automorphisms) = List.fold_right (fun  (x1,x2,x3) (xs1,xs2,xs3)-> ((x1::xs1) , (x2::xs2) ,(x3::xs3) )) 
                   ( find_cycle appr program ( (*find_cycle throws an exception if no cycle is found (for efficiency reasons) *)
          if Location.equal l l' then
            let f (l1,loop,l1') = Location.equal l l1 && Location.equal l' l1' && String.equal (TransitionLabel.update_to_string_rhs transition) (TWNLoop.update_to_string_rhs loop) in
            [[List.find f parallel_edges]]
          else
            (cycles (parallel_edges |> List.filter (fun (l,_,l') -> not (Location.equal l l')) |> Set.of_list) l ([([(l,(TWNLoop.mk_transition transition),l')], (LocationSet.singleton l'))]) [])) )
                  ([],[],[])
        in
        print_string "\n 397 TWN";
        let handled_transitions = ListMonad.(cycle >>= fun (l,twn,l') -> TWNLoop.subsumed_transitions l l' twn) in
        (* let entries = Program.entry_transitions logger program handled_transitions in*)
        (* add_to_proof_graph program handled_transitions entries; *)
        Logger.log logger Logger.INFO (fun () -> "cycle", ["decreasing", Transition.to_id_string (l,transition,l'); "cycle", (TransitionSet.to_id_string (TransitionSet.of_list handled_transitions)); "entry", (TransitionSet.to_id_string (TransitionSet.of_list entries))]);
        let twn_loops = List.map (fun (l,t,l') -> compose_transitions cycle (find l' cycle)) entries in
        Logger.log logger Logger.INFO (fun () -> "twn_loops", List.combine (List.map Transition.to_string entries) (List.map TWNLoop.to_string twn_loops));
        proof_append FormattedString.((mk_header_small (mk_str "TWN-Loops:")) <>
          (List.combine (List.map Transition.to_string_pretty entries) (List.map (TWNLoop.to_string ~pretty:true) twn_loops)
          |> List.map (fun (a,b) -> FormattedString.mk_str_line ("entry: " ^ a) <> FormattedString.mk_block (FormattedString.mk_str_line ("results in twn-loop: " ^ b)))
          |> FormattedString.mappend));
        let global_local_bounds =
          List.map (fun (entry, twn, automorphism) -> (*entry,twn,automorph *)
                let program_only_entry = Program.from ((TransitionSet.to_list (TransitionSet.diff (Program.transitions program) (TransitionSet.of_list entries))) |> List.map List.singleton |> (@) [[entry]]) (Program.start program) in
                let twn_inv = InvariantGeneration.transform_program program_only_entry
                  |> MaybeChanged.unpack
                  |> Program.transitions
                  |> TransitionSet.filter (fun t -> List.exists (Transition.equal t) handled_transitions)
                  |> TransitionSet.to_list
                  |> List.map (TransitionLabel.invariant % Transition.label)
                  |> fun invariants -> List.flatten invariants |> List.filter (fun atom -> List.for_all (List.exists (Atom.equal atom)) invariants)
                  |> TWNLoop.add_invariant twn in
                
    Printf.printf "\n 828 %s" (TWNLoop.to_string twn_inv);
                let eliminated_t = EliminateNonContributors.eliminate_t
                    (TWNLoop.input_vars twn_inv) (TWNLoop.Guard.vars @@ TWNLoop.guard twn_inv) (TWNLoop.update twn_inv) (TWNLoop.remove_non_contributors twn_inv)
                in
    Printf.printf "\n 833 %s \n " (TWNLoop.to_string eliminated_t);
                if VarSet.is_empty (TWNLoop.vars eliminated_t) then
                  Bound.infinity, (entry, Bound.infinity)
                else (
                  let bound = Automorphism.transform_bound automorphism @@ complexity eliminated_t in 
                  if Bound.is_infinity bound then raise (Non_Terminating (handled_transitions, entries));
                  lift appr entry bound, (entry, bound)) )
          (List.map2 (fun (a,b) c -> (a,b,c)) (List.combine entries twn_loops) automorphisms) in (* TODO remove code duplicate from find_cycle*)
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
    print_string "\n 434 TWN";
    let cycle, xs = Option.get opt in
    let bound_with_sizebound = Bound.sum_list (List.map (fun (entry, bound) -> lift appr entry bound) xs) in
    bound_with_sizebound |> tap (fun b -> Logger.log logger Logger.INFO (fun () -> "twn", ["global_bound", Bound.to_string b]))
                         |> tap (fun b -> proof_append FormattedString.((mk_str_line (b |> Bound.to_string ~pretty:true))))))
  |> tap (fun b -> if Bound.compare_asy b (Approximation.timebound appr (l,transition,l')) < 0 then ProofOutput.add_to_proof @@ fun () ->
    FormattedString.((mk_header_big @@ mk_str "Time-Bound by TWN-Loops:") <>
                      mk_header_small (mk_str ("TWN-Loops: t" ^ (TransitionLabel.id transition |> Util.natural_to_subscript) ^ " " ^ Bound.to_string ~pretty:true b)) <>
                      !proof))
