open Batteries
open Formulas
open Polynomials
open ProgramTypes
   
let logger = Logging.(get LocalSizeBound)
           
module Solver = SMT.IncrementalZ3Solver
module Poly = RealPolynomial
module Form = RealFormula
module HelperFuns = LocalSizeBoundHelperFunctions.Make_LocalSizeBoundHelperFunctions(OurFloat) (Poly) (Form)

type kind = [`Lower | `Upper] [@@deriving show, eq]

type bound = {
    kind: kind;
    factor: int option;
    constant: int;
    vars: ([`OneScaled | `ArbScaled] * [`Pos | `Neg] * [`Pure | `Abs]) -> VarSet.t
    [@equal fun a b -> List.cartesian_product [`Pos; `Neg] [`Pure; `Abs] 
                      |> List.cartesian_product [`OneScaled; `ArbScaled]
                      |> List.for_all (fun (x,(y,z)) -> VarSet.equal (a (x,y,z)) (b (x,y,z))) ];
  } [@@deriving eq]

type t = 
    [
      `Unbounded of kind
    | `Bounded of bound
  ] [@@deriving eq]


let scaling_to_string scaling = 
  match scaling with
    | `OneScaled -> "onescaled"
    | `ArbScaled -> "arbscaled"
  
let compare_bounds t1 t2 =
  match (t1, t2) with
  | (`Unbounded `Lower, _) -> true
  | (`Unbounded `Upper, _) -> false
  | (_, `Unbounded `Lower) -> false
  | (_, `Unbounded `Upper) -> true
  | (`Bounded lsb1, `Bounded lsb2) ->
    if lsb1.factor == lsb2.factor then
      lsb1.constant < lsb2.constant
    else
      lsb1.factor < lsb2.factor
  
let is_finite_bound t =
  match t with
  | `Unbounded _ -> false
  | `Bounded _ -> true

let mk ?(s=1) ?(c=0) ?(pos_one_abs=[]) ?(pos_arb_abs=[]) ?(pos_one_pure=[]) ?(pos_arb_pure=[]) ?(neg_one_abs=[]) ?(neg_arb_abs=[]) ?(neg_one_pure=[]) ?(neg_arb_pure=[]) kind = 
  `Bounded {
    kind;
    factor = if s = 1 then None else Some s;
    constant = c;
    vars =
      if s = 1 then
        function
          | (`OneScaled, `Pos, `Pure) -> VarSet.union (VarSet.of_string_list pos_one_pure) (VarSet.of_string_list pos_arb_pure)
          | (`OneScaled, `Pos, `Abs) -> VarSet.union (VarSet.of_string_list pos_one_abs) (VarSet.of_string_list pos_arb_abs)
          | (`OneScaled, `Neg, `Pure) -> VarSet.union (VarSet.of_string_list neg_one_pure) (VarSet.of_string_list neg_arb_pure) 
          | (`OneScaled, `Neg, `Abs) -> VarSet.union (VarSet.of_string_list neg_one_abs) (VarSet.of_string_list neg_one_abs)
          | _ -> VarSet.empty
      else
        function
          | (`ArbScaled, `Pos, `Pure) -> VarSet.of_string_list pos_arb_pure
          | (`ArbScaled, `Pos, `Abs) -> VarSet.of_string_list pos_arb_abs
          | (`ArbScaled, `Neg, `Pure) -> VarSet.of_string_list neg_arb_pure
          | (`ArbScaled, `Neg, `Abs) -> VarSet.of_string_list neg_arb_abs
          | (`OneScaled, `Pos, `Pure) -> VarSet.of_string_list pos_one_pure 
          | (`OneScaled, `Pos, `Abs) -> VarSet.of_string_list pos_one_abs 
          | (`OneScaled, `Neg, `Pure) -> VarSet.of_string_list neg_one_pure 
          | (`OneScaled, `Neg, `Abs) -> VarSet.of_string_list neg_one_abs 
  }
       
let factor_bound lsb =
  lsb.factor
  
let factor bound =
  match bound with
  | `Unbounded _ -> raise (Failure "Unbounded, no factor exists")
  | `Bounded lsb -> factor_bound lsb
  
let constant_bound lsb =
  lsb.constant
  
let constant lsb =
  match lsb with
  | `Unbounded _ -> raise (Failure "Unbounded, no constant exists")
  | `Bounded b -> constant_bound b

let vars_of_scaling scaling lsb = 
  List.cartesian_product [`Pos;`Neg] [`Pure;`Abs]
  |> List.map (fun (pa,pu) -> lsb.vars (scaling,pa,pu) )
  |> List.fold_left (fun vs1 vs2 -> VarSet.union vs1 vs2) VarSet.empty
  
let vars_of_sign sign t =
  match t with
  | `Unbounded _ -> VarSet.empty
  | `Bounded { vars; _ } ->
      List.cartesian_product [`ArbScaled; `OneScaled] [`Pure; `Abs]
      |> List.map (fun (a,b) -> (a,sign,b))
      |> List.fold_left (fun vs arg -> VarSet.union vs (vars arg)) VarSet.empty

let vars_of_purity scaling purity t =
  match t with
  | `Unbounded _ -> VarSet.empty
  | `Bounded { vars; _ } ->
      [`Neg; `Pos]
      |> List.map (fun b -> (scaling,b,purity))
      |> List.fold_left (fun vs arg -> VarSet.union vs (vars arg)) VarSet.empty

let vars t =
  match t with
  | `Unbounded _ -> VarSet.empty
  | bounded ->
    VarSet.union (vars_of_sign `Pos bounded) (vars_of_sign `Neg bounded)
          
let multiplier = function
  | `Pos -> identity
  | `Neg -> Bound.neg

let absifier = function
  | _, _, `Pure -> identity
  | `Upper, `Pos, `Abs -> Bound.(max zero)
  | `Upper, `Neg, `Abs -> Bound.(min zero)
  | `Lower, `Pos, `Abs -> Bound.(min zero)
  | `Lower, `Neg, `Abs -> Bound.(max zero)

let pre_kind = function
  | `Upper, `Pos -> `Upper
  | `Upper, `Neg -> `Lower
  | `Lower, `Pos -> `Lower
  | `Lower, `Neg -> `Upper
  
let sum_vars substitution (vars: VarSet.t) (kind, sign, purity): Bound.t =
  vars
  |> VarSet.enum
  |> Enum.map Bound.of_var
  |> Enum.map (absifier (kind, sign, purity))
  |> Enum.map (multiplier sign)
  |> Bound.sum
  |> ((kind, sign) |> pre_kind |> substitution |> Bound.substitute_f)
  
let as_substituted_bound substitution t =
  match t with
  | `Unbounded `Upper -> Bound.infinity
  | `Unbounded `Lower -> Bound.minus_infinity
  | `Bounded lsb ->
    let variables scale =
      List.cartesian_product [`Pos; `Neg] [`Pure; `Abs]
      |> List.map (fun (sign, purifier) -> sum_vars substitution (lsb.vars (scale,sign, purifier)) (lsb.kind, sign, purifier))
      |> List.enum
      |> Bound.sum
    in
    let one_scaled_part = Bound.(variables `OneScaled + (of_int lsb.constant)) in
    match lsb.factor with
      | None -> one_scaled_part
      | Some factor ->
          Bound.(one_scaled_part + (of_int factor) * (variables `ArbScaled))

let as_bound =
  as_substituted_bound (fun _ -> Bound.of_var)
  
let default = function
  | `Upper -> Bound.infinity
  | `Lower -> Bound.minus_infinity
  
  
let to_string t =
  match t with
  | `Unbounded `Lower -> "-infinity"
  | `Unbounded `Upper -> "infinity"
  | `Bounded lsb ->
      let arbscaledstrings = 
        match lsb.factor with
          | None -> []
          | Some factor -> [String.of_int factor;
                            VarSet.to_string (lsb.vars (`ArbScaled,`Pos,`Pure));
                            VarSet.to_string (lsb.vars (`ArbScaled,`Pos,`Abs));
                            VarSet.to_string (lsb.vars (`ArbScaled,`Neg,`Pure));
                            VarSet.to_string (lsb.vars (`ArbScaled,`Neg,`Abs))]
      in
      String.concat " " (["ExpectedScaledSum"] @ arbscaledstrings @
                         [String.of_int lsb.constant;
                          VarSet.to_string (lsb.vars (`OneScaled, `Pos, `Pure));
                          VarSet.to_string (lsb.vars (`OneScaled, `Pos, `Abs));
                          VarSet.to_string (lsb.vars (`OneScaled, `Neg, `Pure));
                          VarSet.to_string (lsb.vars (`OneScaled, `Neg, `Abs))])

(* [x;y] -> [x+y; x+0; 0+y; 0+0] *)
let abs_vars_combinations vars =
  vars
  |> VarSet.map_to_list Polynomial.of_var
  |> List.map (fun v -> [v; Polynomial.zero])
  |> List.n_cartesian_product  
  |> List.map (Polynomial.sum % List.enum)

let pure_vars_sum scaling t =
  match t with
  | `Unbounded _ -> Polynomial.zero
  | `Bounded lsb ->
    let sum_up vars =
      vars
      |> VarSet.map_to_list Polynomial.of_var
      |> List.enum
      |> Polynomial.sum
    in Polynomial.(sum_up (lsb.vars (scaling, `Pos, `Pure)) - sum_up (lsb.vars (scaling, `Neg, `Pure)))

let as_formula in_v t =
  match t with
  | `Unbounded _ -> Formula.mk_true
  | `Bounded lsb ->
    let v = Polynomial.of_var in_v in
    List.cartesian_product [`OneScaled, `ArbScaled] [`Pos,`Neg]
    |> List.map (fun (s,p) -> abs_vars_combinations (lsb.vars (`OneScaled,`Neg,`Abs)) )
    |> List.n_cartesian_product
    |> List.map (fun l ->
                   match l with 
                     | [a;b;c;d] -> (a,b,c,d)
                     | otherwise -> failwith "impossible"
                )
    |> List.map 
         (fun
           (one_pos,one_neg,arb_pos,arb_neg) -> 
             let onescaled = 
               Polynomial.(of_int lsb.constant + pure_vars_sum `OneScaled t + one_pos - one_neg)
             in
             let comp =
               match lsb.kind with 
                 | `Upper -> Formula.Infix.(<=)
                 | `Lower -> Formula.Infix.(>=)
             in
             match lsb.factor with
               | None -> comp v onescaled
               | Some factor -> Polynomial.(comp v (onescaled + of_int factor * (pure_vars_sum `OneScaled t + arb_pos - arb_neg)))
        )
    |> Formula.any

let is_bounded_with solver var lsb =
  Solver.push solver;
  Solver.add solver (Formula.neg (as_formula var lsb));
  let result = Solver.unsatisfiable solver in
  Solver.pop solver;
  result

let unabsify scaling var t =
  match t with
  | `Bounded lsb -> 
    if VarSet.mem var (lsb.vars (scaling,`Pos, `Abs)) then
      `Bounded { lsb with vars = function
                        | (scaling,`Pos, `Abs) -> VarSet.remove var (lsb.vars (scaling, `Pos, `Abs))
                        | (scaling,`Pos, `Pure) -> VarSet.add var (lsb.vars (scaling, `Pos, `Pure))
                        | arg -> lsb.vars arg
      }
    else
      `Bounded { lsb with vars = function
                        | (scaling, `Neg, `Abs) -> VarSet.remove var (lsb.vars (scaling, `Neg, `Abs))
                        | (scaling, `Neg, `Pure) -> VarSet.add var (lsb.vars (scaling, `Neg, `Pure))
                        | arg -> lsb.vars arg
      }
  (* if the local size bound is unbounded, than nothing happens *)
  | t -> t
 
(** Tries to convert conservatively choosed absolute values of variables by the variables themself. *)
let unabsify_vars scaling (p: t -> bool) (bound: t): t =
  let execute () =
    match bound with
    | `Bounded lsb ->
      let unabsify_with_candidate var current_lsb =
        let possible_better_lsb = unabsify scaling var current_lsb in
        if p possible_better_lsb then
          possible_better_lsb
        else
          current_lsb
      in
      VarSet.fold unabsify_with_candidate (vars_of_purity scaling `Abs bound) bound
    | bound -> bound
  in
  Logger.with_log logger Logger.DEBUG
                  (fun () -> "unabsify_vars", ["scaling",scaling_to_string scaling;"lsb", to_string bound])
                  ~result:to_string
                  execute
  
(** Minimizes the given variable set, such that the predicate p is still satisfied.
    We assume that the given variable set satisfies the predicate p.
    TODO Currently we use an arbitrary order. This is sound, however for "x <= y && y <= z" we may return z although y would be definitely better. *)
let minimize_vars (p: VarSet.t -> bool) (vars: VarSet.t): VarSet.t =
  let execute () =
    (** Removes the candidate from the set, if the candidate is not necessary. *)
    let minimize_with_candidate var current_minimized_set =
      let further_minimized_set = VarSet.remove var current_minimized_set in
      if p further_minimized_set then
        further_minimized_set
      else
        current_minimized_set
    in
    VarSet.fold minimize_with_candidate vars vars
  in
  Logger.with_log logger Logger.DEBUG
                  (fun () -> "minimize_vars", ["vars", VarSet.to_string vars])
                  ~result:VarSet.to_string
                  execute
  
let minimize_scaledsum_vars (p: t -> bool) (bound: t): t =
  match bound with
  | `Bounded lsb ->
    let minimize_vars scaling sign =
      minimize_vars
        (fun vars ->
          p (`Bounded { lsb with
              vars = function
                    | (sc, s, `Abs) when s = sign && sc = scaling -> vars
                    | arg -> lsb.vars arg
            })
        ) (lsb.vars (scaling, sign, `Abs))
    in
    (* For Performance *)
    let minimized_one_pos_vars = minimize_vars `OneScaled `Pos
    and minimized_arb_pos_vars = minimize_vars `ArbScaled `Pos
    and minimized_one_neg_vars = minimize_vars `OneScaled `Neg 
    and minimized_arb_neg_vars = minimize_vars `ArbScaled `Neg in
    `Bounded { lsb with vars = function
                      (* TODO Maybe we have to do first one sign, then the other instead of do it parallel. *)
                      | (`OneScaled, `Pos, `Abs) -> minimized_one_pos_vars
                      | (`ArbScaled, `Pos, `Abs) -> minimized_arb_pos_vars
                      | (`OneScaled, `Neg, `Abs) -> minimized_one_neg_vars
                      | (`ArbScaled, `Neg, `Abs) -> minimized_arb_neg_vars
                      | (sc, sign, `Pure) -> lsb.vars (sc, sign, `Pure)
    }
  | bound -> bound

let move_var_to_scaling_one p purity var lsb = 
  { 
    lsb with vars = 
               function
                 | (`ArbScaled, a, b) when a = p && b = purity -> VarSet.remove var (lsb.vars (`ArbScaled, p, purity))
                 | (`OneScaled, a, b) when a = p && b = purity -> VarSet.add var (lsb.vars (`ArbScaled, p, purity))
                 | (a,b,c) -> lsb.vars (a,b,c)
  }

let move_vars_to_scaling_one (p: t -> bool) t =
  match t with
    | `Bounded lsb ->
        let move_vars_wrt_varset pa purity = 
          lsb.vars (`ArbScaled,pa,purity)
          |> (fun vs -> 
                VarSet.fold (fun var lsb' ->
                               let newlsb = move_var_to_scaling_one pa purity var lsb' in
                               if p (`Bounded newlsb) then
                                 newlsb
                               else
                                 lsb'
                            ) vs lsb)
        in
        let newlsb = List.cartesian_product [`Pos;`Neg] [`Pure;`Abs]
                     |> List.fold_left (fun lsb' (pa,purity) -> move_vars_wrt_varset pa purity) lsb
        in
        if VarSet.is_empty (vars_of_scaling `ArbScaled newlsb) then
          `Bounded {newlsb with factor = None}
        else
          `Bounded newlsb
    | t -> t

let optimize_c (range: int) (p: t -> bool) (bound: t): t =
  let execute () = 
    match bound with 
    | `Bounded lsb ->
      `Bounded { lsb with constant = match lsb.kind with
                          | `Upper -> HelperFuns.binary_search ~divisor:64. (-range) range (fun c -> p (`Bounded { lsb with constant = c }))
                          | `Lower -> - (HelperFuns.binary_search ~divisor:64. (-range) range (fun c -> p (`Bounded { lsb with constant = -c })))
      }
    | bound -> bound
  in
  Logger.with_log logger Logger.DEBUG
                  (fun () -> "optimize_c", ["lowest", Int.to_string (-range); "highest", Int.to_string range; "lsb", to_string bound])
                  ~result:to_string
                  execute

let optimize_s (lowest: int) (highest: int) (p: t -> bool) (bound: t): t =
  let execute () = 
    match bound with 
    | `Bounded lsb ->
      `Bounded {lsb with factor = Some (HelperFuns.binary_search ~divisor:16.
                          lowest
                          highest
                          (fun s -> p (`Bounded { lsb with factor = Some s })) )
      }
    | bound -> bound
  in
  Logger.with_log logger Logger.DEBUG
                  (fun () -> "optimize_s", ["lowest", Int.to_string lowest; "highest", Int.to_string highest; "lsb", to_string bound])
                  ~result:to_string
                  execute

let initial_lsb kind (f: int) (constant: int) (vars: VarSet.t) = 
  `Bounded {
    kind;
    factor = Some f;
    constant = (
      match kind with
      | `Upper -> constant
      | `Lower -> -constant
    );
    vars = 
      if f = 1 then
        function
           | (`OneScaled, _, `Abs) -> vars
           | _ -> VarSet.empty
      else
        function 
          | (`ArbScaled, _, `Abs) -> vars
          | _ -> VarSet.empty
  }


(* Check if x <= s * (c + [v1,...,vn]). *)
let find_scaled_bound kind program_vars solver var guard_vars update_vars (s: int) (c: int) =
  let execute () =
    let is_bounded = is_bounded_with solver var in
    Enum.seq 0 ((+) 1) ((>) (VarSet.cardinal update_vars + 1))
    |> Util.find_map (fun count ->
           VarSet.combinations count program_vars
           |> List.enum
           |> Enum.filter (fun vars -> VarSet.subset vars update_vars)
           |> Enum.map (initial_lsb kind s c)
           |> Enum.filter is_bounded
           |> Enum.map (optimize_s 1 s is_bounded)
           |> Enum.map (optimize_c c is_bounded)
           |> Enum.map (minimize_scaledsum_vars is_bounded)
           |> Enum.map (unabsify_vars `ArbScaled is_bounded)
           |> Enum.map (unabsify_vars `OneScaled is_bounded)
           |> Enum.map (move_vars_to_scaling_one is_bounded)
           |> Util.min_option compare_bounds
         )
    |>  Option.default (`Unbounded kind)
  in Logger.with_log logger Logger.DEBUG
                  (fun () -> "find_scaled_bound", ["var", Var.to_string var; "guard_vars", VarSet.to_string guard_vars])
                  ~result:(Bound.to_string % as_bound)
                  execute

let find_bound kind program_vars var formula update_vars s_range =
  let execute () =
    let solver = Solver.create ~model:false () in
    let c_range_ = (HelperFuns.c_range formula) in
    Solver.add_real solver formula;
    find_scaled_bound kind program_vars solver var (formula |> Form.vars |> VarSet.remove var) update_vars s_range c_range_
  in Logger.with_log logger Logger.DEBUG
                     (fun () -> "find_local_size_bound", [
                          "kind", show_kind kind;
                          "var", Var.to_string var;
                          "formula", Form.to_string formula;
                          "s_range", string_of_int s_range;
                          "c_range", string_of_int (HelperFuns.c_range formula)])
                     ~result:(Bound.to_string % as_bound)
                     execute

(** Internal memoization for local size bounds *)
module LSB_Cache =
  Hashtbl.Make(
      struct
        type t = kind * GeneralTransition.t * Var.t * Location.t
        let equal (k1,gt1,v1,l1) (k2,gt2,v2,l2) =
          equal_kind k1 k2
          && GeneralTransition.same gt1 gt2
          && Var.equal v1 v2
          && Location.equal l1 l2
        let hash (k,gt,v,l) =
          Hashtbl.hash (k, GeneralTransition.id gt, v, l)
      end
    )
   
let (table: t Option.t LSB_Cache.t) =
  LSB_Cache.create 10
  
let reset () =
  LSB_Cache.clear table

let compute_single_local_size_bound program kind gt var l =
  let trans_reaching_l = 
    GeneralTransition.transitions gt
    |> TransitionSet.filter (Location.equal l % Transition.target)
  in
  let prob_reaching_l = 
    trans_reaching_l
    |> (fun ts -> TransitionSet.fold ((+.) % TransitionLabel.probability % Transition.label) ts 0.0)
  in
  let handle_update_element ue =
    match (ue) with
      | TransitionLabel.UpdateElement.Poly p -> 
          Poly.of_intpoly(p)
      | TransitionLabel.UpdateElement.Dist d ->
          ProbDistribution.expected_value d
  in
  let lsb =
    (* If we have an update pattern, it's like x'=b and therefore x'<=b and x >=b and b is a bound for both kinds. *)
    trans_reaching_l
    |> TransitionSet.to_list
    |> List.map 
         (fun t ->
           TransitionLabel.update (Transition.label t) var
           |> Option.map (fun u -> (Transition.label t,u)))
    |> Util.option_sequence
    |> Option.map 
         (fun labelsus -> List.map (fun (label,ue) -> Poly.mul (Poly.of_constant ((TransitionLabel.probability label) /.
                                      prob_reaching_l)) (handle_update_element ue)) labelsus
                          |> List.fold_left Poly.add Poly.zero)
    |> Option.map (fun update ->
           (* Introduce a temporary result variable *)
           let v' = Var.fresh_id Var.Real () in
           let update_vars = Poly.vars update in
           let guard_with_update = 
             Form.Infix.(Form.mk (Constraints.RealConstraint.of_intconstraint (GeneralTransition.guard gt)) && Poly.of_var v' = update)
           in
(*         A local size bound must not depend on temporary variables    *)
           find_bound kind (Program.input_vars program) v' guard_with_update update_vars 
                      (HelperFuns.s_range update)
         )
  in
  LSB_Cache.add table (kind,gt,var,l) lsb;
  (Logger.log logger Logger.INFO
     (fun () -> "add_local_size_bound", [
          "kind", show_kind kind;
          "transition", GeneralTransition.to_string gt;
          "variable", Var.to_string var;
          "lsb", Util.option_to_string (Bound.to_string % as_bound) lsb]))

let compute_local_size_bounds program =
  program
  |> Program.transitions
  |> GeneralTransitionSet.from_transitionset
  |> GeneralTransitionSet.enum
  |> Enum.cartesian_product (program |> Program.vars |> VarSet.enum)
  |> Enum.cartesian_product ([`Lower; `Upper] |> List.enum)
  |> Enum.iter (fun (kind, (v,gt)) ->
         GeneralTransition.targets gt
         |> LocationSet.iter (compute_single_local_size_bound program kind gt v)
       )
  
let sizebound_local program kind gt v l =
  if LSB_Cache.is_empty table then
    compute_local_size_bounds program;
  try
    LSB_Cache.find table (kind, gt, v, l)
  with Not_found ->
    raise (Failure "Non-existing local size bound requested!")

let sizebound_local_rv program kind (gt,v,l) =
  sizebound_local program kind gt v l
  
let sizebound_local_scc program kind scc =
  if scc
     |> List.map (sizebound_local_rv program kind)
     |> List.for_all Option.is_some
  then
    Some (fun kind (gt,v) tl -> Option.get (sizebound_local_rv program kind (gt,v,tl)))
  else None
