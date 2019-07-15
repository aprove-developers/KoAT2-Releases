open Batteries
open Polynomials
open Atoms
open Constraints
open ProgramTypes
open Formulas

let logger = Logging.(get Time)

type measure = [ `Cost | `Time ] [@@deriving show, eq]

(** All entry transitions of the given transitions.
    These are such transitions, that can occur immediately before one of the transitions, but are not themselves part of the given transitions. *)
let entry_transitions (program: Program.t) (rank_transitions: Transition.t list): Transition.t List.t =
  rank_transitions
  |> List.enum
  |> Enum.map (Program.pre program)
  |> Enum.flatten
  |> Enum.filter (fun r ->
         rank_transitions
         |> List.enum
         |> Enum.for_all (not % Transition.same r)
       )
  |> Enum.uniq_by Transition.same
  |> List.of_enum
  |> tap (fun transitions -> Logger.log logger Logger.DEBUG
                               (fun () -> "entry_transitions", ["result", transitions |> List.enum |> Util.enum_to_string Transition.to_id_string]))

let apply (get_sizebound: [`Lower | `Upper] -> Transition.t -> Var.t -> Bound.t) (rank: Polynomial.t) (transition: Transition.t): Bound.t =
  rank
  |> Bound.of_poly
  |> Bound.appr_substitution
       `Upper
       ~lower:(get_sizebound `Lower transition)
       ~higher:(get_sizebound `Upper transition)


(* method transforms polynome to parapolynom*)
let as_parapoly label var =
    match TransitionLabel.update label var with
    (** Correct? In the nondeterministic case we just make it deterministic? *)
    | None -> ParameterPolynomial.of_var var
    | Some p -> ParameterPolynomial.of_polynomial p

(* Compute new Timebounds for MRFs*)

module Valuation = Valuation.Make(OurInt)

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
let rec compute_coefficients (degree:int) (rank : (Location.t -> Polynomial.t) list) (l,t,l') list =
  match list with
  | [] -> compute_coefficients degree rank (l,t,l') [(1.,1.)]
  | _  ->
    if degree == List.length list then list
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
       compute_coefficients (degree:int) rank (l,t,l') (List.append list [(ck,dk)])

(* Returns max_i c_i /. d_i *)
let rec maximum_coefficients list =
  match list with
  | [(x,y)] -> int_of_float(ceil (x /. y))
  | (x,y) :: rest -> max (int_of_float (ceil  (x /. y))) (maximum_coefficients rest)
  | _ -> 0

(* Constructs the nested max bounds of all functions of the mrf*)
let rec maxBound_of_list list =
 match list with
 | [] -> Bound.zero
 | [x] -> x
 | x::xs -> Bound.max x (maxBound_of_list xs)

(* computes new bounds*)
let compute_bound_mrf (appr: Approximation.t) (program: Program.t) (rank: MultiphaseRankingFunction.t): Bound.t =
 let execute () =
   rank
   |> MultiphaseRankingFunction.non_increasing
   |> entry_transitions program
   |> List.enum
   |> Enum.map (fun (l,t,l') ->
       let decreasing = MultiphaseRankingFunction.decreasing rank in
       let timebound = Approximation.timebound appr (l,t,l') in 
       let coefficients = (compute_coefficients (MultiphaseRankingFunction.degree rank) (MultiphaseRankingFunction.rank rank) decreasing  []) in
         let maximum_coefficient = (maximum_coefficients coefficients) in
         let evaluate = (fun rank -> (apply (fun kind -> Approximation.sizebound kind appr) rank) (l,t,l')) in
         let var = (List.init (MultiphaseRankingFunction.degree rank) (fun i -> (evaluate ((List.nth (MultiphaseRankingFunction.rank rank) i) l')))) in
         let rhs = Bound.(add one (max one (mul (of_int maximum_coefficient)  (maxBound_of_list var)))) in
          Bound.(
            if is_infinity timebound then
              if equal zero rhs then
                zero
              else
                infinity
            else
              if is_infinity rhs then
                infinity
              else
                timebound * rhs
          ))
   |> Bound.sum
 in Logger.with_log logger Logger.DEBUG
      (fun () -> "compute_bound", ["decreasing", Transition.to_id_string (MultiphaseRankingFunction.decreasing rank);
                                   "non_increasing", Util.enum_to_string Transition.to_id_string (List.enum (MultiphaseRankingFunction.non_increasing rank));
                                   "rank", MultiphaseRankingFunction.only_rank_to_string rank;])
                    ~result:Bound.to_string
                    execute


 let compute_bound (appr: Approximation.t) (program: Program.t) (rank: RankingFunction.t): Bound.t =
   let execute () =
     rank
     |> RankingFunction.non_increasing
     |> entry_transitions program
     |> List.enum
     |> Enum.map (fun (l,t,l') ->
            let timebound = Approximation.timebound appr (l,t,l') in
            let rhs = Bound.(max zero (apply (fun kind -> Approximation.sizebound kind appr) (RankingFunction.rank rank l') (l,t,l'))) in
            Bound.(
              if is_infinity timebound then
                if equal zero rhs then
                  zero
                else
                  infinity
              else
                if is_infinity rhs then
                  infinity
                else
                  timebound * rhs
            ))
     |> Bound.sum
   in Logger.with_log logger Logger.DEBUG
        (fun () -> "compute_bound", ["decreasing", Transition.to_id_string (RankingFunction.decreasing rank);
                                     "non_increasing", Util.enum_to_string Transition.to_id_string (List.enum (RankingFunction.non_increasing rank));
                                     "rank", RankingFunction.only_rank_to_string rank])
                      ~result:Bound.to_string
                      execute

let add_bound = function
  | `Time -> Approximation.add_timebound
  | `Cost -> Approximation.add_costbound

let improve_with_rank measure program appr rank =
  let bound = compute_bound appr program rank in
  if Bound.is_infinity bound then
    MaybeChanged.same appr
  else
    rank
    |> RankingFunction.decreasing
    |> (fun t -> add_bound measure bound t appr)
    |> MaybeChanged.changed

  let improve_with_rank_mrf measure program appr rank =
    let bound = compute_bound_mrf appr program rank in
    if Bound.is_infinity bound then
      MaybeChanged.same appr
    else
      rank
      |> MultiphaseRankingFunction.decreasing
      |> (fun t -> add_bound measure bound t appr)
      |> MaybeChanged.changed

(** Checks if a transition is bounded *)
let bounded measure appr transition =
  match measure with
  | `Time -> Approximation.is_time_bounded appr transition
  | `Cost -> false

let improve  ?(mrf = false) measure program appr  =
  let execute () =
    program
    |> Program.non_trivial_transitions
    |> TransitionSet.filter (fun t -> not (bounded measure appr t))
    |> TransitionSet.enum
    |> MaybeChanged.fold_enum (
      if (mrf && measure <> `Cost) then
      (fun appr transition ->
           MultiphaseRankingFunction.find  measure program transition
           |> List.enum
           |> MaybeChanged.fold_enum (fun appr rank ->
                  improve_with_rank_mrf measure program appr rank
             ) appr)
      else
      (fun appr transition ->
           RankingFunction.find measure program transition
           |> List.enum
           |> MaybeChanged.fold_enum (fun appr rank ->
                  improve_with_rank measure program appr rank
             ) appr)

         ) appr
  in Logger.with_log logger Logger.INFO
                     (fun () -> "improve_bounds", ["measure", show_measure measure])
                     execute
