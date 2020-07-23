open Batteries
open Polynomials
open Atoms
open Constraints
open ProgramTypes
open Formulas

let logger = Logging.(get Time)

module Valuation = Valuation.Make(OurInt)

(* Method transforms polynome to parapolynom*)
let as_parapoly label var =
    match TransitionLabel.update label var with
    (** Correct? In the nondeterministic case we just make it deterministic? *)
    | None -> ParameterPolynomial.of_var var
    | Some p -> ParameterPolynomial.of_polynomial p

(*Generates formula for SMT Solver for mues *)
let template_formula (rank : (Location.t -> Polynomial.t) list) (k:int) (l,t,l') newVariables =
  (*generate templates*)
  let polynomials = (List.init k (fun i -> 
    (ParameterPolynomial.mult_with_const (Polynomial.of_var (List.nth newVariables i)) (ParameterPolynomial.of_polynomial ((List.nth rank i) l))))) in
  let f_k = (ParameterPolynomial.of_polynomial (((List.nth rank k) l))) in
  let f_k_update = (ParameterPolynomial.substitute_f (as_parapoly t) (ParameterPolynomial.of_polynomial((List.nth rank k) l'))) in
  let last_poly = ParameterPolynomial.sub f_k f_k_update in
  let complete_poly = (List.fold_left (fun a b -> (ParameterPolynomial.add a b)) ParameterPolynomial.zero (last_poly::polynomials)) in
  (*Built formula*)
  let atom = ParameterAtom.Infix.(complete_poly >= ParameterPolynomial.one) in 
  let formula = Formula.mk (ParameterConstraint.farkas_transform (TransitionLabel.guard t) atom ) in 

  let formulas_mues_positve = List.init k (fun i -> 
                    (Formula.lift (Atom.Infix.(Polynomial.of_var(List.nth newVariables i) >= Polynomial.zero)))) in
  let formula_mues_positve = List.fold_left 
                        (fun a b -> Formula.mk_and a b) 
                              (Formula.lift (Atom.Infix.(Polynomial.of_var(List.nth newVariables (k - 1)) >= Polynomial.one))) 
                              formulas_mues_positve in
  Formula.mk_and formula_mues_positve formula

(*Calculates mues for coefficients  1,...,k - 1 *)
let compute_mue_k (rank : (Location.t -> Polynomial.t) list) (k:int) (l,t,l') =  
    (*Use SMT Solver *)
    let newVariables = Var.fresh_id_list Var.Int k in
    let formula = template_formula rank k (l,t,l') newVariables in 
    let opt = SMT.IncrementalZ3Solver.create () in
    SMT.IncrementalZ3Solver.add opt formula;
    let model = SMT.IncrementalZ3Solver.model opt in
    List.init k (fun i ->  OurInt.to_float (Valuation.eval_opt (List.nth newVariables i) (Option.get model) |? OurInt.zero))

(* Returns sum_i (list1(i) := (c_i,d_i)_i -> c_i )* (list2(i) := (mu_i)_i) *)
let rec sumProduct (list1, list2) =
  match (list1,list2) with
    | ([(x,_)],[y]) -> x *. y
    | ((x,_)::xs,y::ys) -> x *. y +. sumProduct (xs, ys)
    | _ -> 0.

(* Calculates recursive all coefficient c_k,d_k*)
let rec compute_coefficients (depth:int) (rank : (Location.t -> Polynomial.t) list) (l,t,l') list =
  match list with
  | [] -> compute_coefficients depth rank (l,t,l') [(1.,1.)]
  | _  ->
    if depth == List.length list then list
     else
       let k = List.length list in
       let mues = compute_mue_k rank k (l,t,l') in
       let sum_mues = (List.fold_left (fun a b -> a +. b) 0. mues) in
       let ck = (1. +. sum_mues) +. (sumProduct (list, mues)) /. (float k) +. (List.nth mues (k - 1)) *.   (snd (List.nth list (k - 1))) in
       let dk = (List.nth mues (k - 1)) *. (snd (List.nth list (k - 1))) /. (float (k + 1)) in
       Logger.log logger Logger.DEBUG (fun () -> "coef.: ", ["k" , string_of_int k;
                                                            "mues", (List.fold_left (fun a b -> a ^ " " ^ string_of_float b) "" mues);
                                                            ("c_" ^ string_of_int k) , string_of_float ck;
                                                            ("d_" ^ string_of_int k), string_of_float dk;
                                                            ("c_" ^ string_of_int k ^ "/d_" ^ string_of_int k), string_of_int(int_of_float (ceil  (ck /. dk)))]);
       compute_coefficients (depth:int) rank (l,t,l') (List.append list [(ck,dk)])

(* Returns max_i c_i /. d_i *)
let rec maximum_coefficients list =
  match list with
  | [(x,y)] -> int_of_float(ceil (x /. y))
  | (x,y) :: rest -> max (int_of_float (ceil  (x /. y))) (maximum_coefficients rest)
  | _ -> 0

(* Constructs the nested max bounds of all functions of the MPRF*)
let rec maxBound_of_list list =
 match list with
 | [] -> Bound.one
 | [x] ->  Bound.max x (Bound.one)
 | x::xs -> Bound.max x (maxBound_of_list xs)

let coefficient (rank: MultiphaseRankingFunction.t) = 
  let decreasing = MultiphaseRankingFunction.decreasing rank in
  let coefficients = (compute_coefficients (MultiphaseRankingFunction.depth rank) (MultiphaseRankingFunction.rank rank) decreasing  []) in
  maximum_coefficients coefficients