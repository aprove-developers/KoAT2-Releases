open Batteries
open Polynomials
open Formulas
open BoundsInst

module Guard = Constraints.Constraint
type polynomial = Polynomial.t
module VarMap = Map.Make(Var)

exception RecursionNotSupported
exception OnlyCom1Supported
exception ProbabilitiesNotBetweenZeroAndOne
exception DifferentUpdatesAndProbabilities

type kind = [ `Lower | `Upper ] [@@deriving eq, ord]

type trans_id_counter = (int ref * int ref)

let new_trans_id_counter = fun () -> (ref 0, ref 0)

let get_id_counter = Tuple2.first
let get_gt_id_counter = Tuple2.second

let get_unique_by_ref r () =
  let value = !r in
  r := !r + 1;
  value

let get_unique_id counter =
  get_unique_by_ref (get_id_counter counter)

let get_unique_gt_id counter =
  get_unique_by_ref (get_gt_id_counter counter)

module UpdateElement =
  struct
    type t = Poly of Polynomial.t | Dist of ProbDistribution.t [@@deriving eq,ord]

    let mk_identity v =
      Poly (Polynomial.of_var v)

    let to_string u =
      match u with
        | Poly p -> "(Polynomial: " ^ (Polynomial.to_string p)  ^ ")"
        | Dist d -> "(Distribution: " ^ (ProbDistribution.to_string d) ^ ")"

    let to_short_string u =
      match u with
        | Poly p -> Polynomial.to_string p
        | Dist d -> ProbDistribution.to_string d

    let rename rename_map u =
      match u with
        | Poly p -> Poly (Polynomial.rename rename_map p)
        | Dist d -> Dist (ProbDistribution.rename rename_map d)

    let vars v u =
      match u with
        | Poly p -> Polynomial.vars p
        | Dist d -> VarSet.union (VarSet.singleton v) (ProbDistribution.vars d)

    let is_polynomial u =
      match u with
        | Poly p -> true
        | otherwise -> false

  let substitute sub u =
    match u with
      | Poly p -> Poly (Polynomial.substitute_f sub p)
      | Dist d -> Dist (ProbDistribution.substitute sub d)
  end

(*
  Note that while an argument is uniquely determined by a place in its term
  our variables will in general be named (achievable for example using the simple input).
  Thus, to maintain the relation between an variables name and its place we have two lists
  inputs_vars_ordered and output_vars_ordered
*)
type t = {
    id : int;
    gt_id: int;
    input_vars_ordered: Var.t List.t;
    update : UpdateElement.t VarMap.t;
    update_vars_ordered : Var.t List.t;
    guard : Guard.t;
    guard_without_invariants: Guard.t;
    invariants: Guard.t;
    cost : Polynomial.t;
    gtcost : RealBound.t;
    probability : OurFloat.t;
  }

let triples (list1) (list2) (list3) = List.map (fun ((x,y),z) -> (x,y,z)) (List.combine (List.combine list1 list2) list3)

let quatruples (list1) (list2) (list3) (list4) = List.map (fun ((x,y),(z,w)) -> (x,y,z,w)) (List.combine (List.combine list1 list2) (List.combine list3 list4))

(* Generates a probabilistic label, needs a name to distinguish different labels belonging to the same transition *)
let make_prob id_counter ?(cvect = (Polynomial.one, RealBound.one)) com_kind ~input_vars_ordered ~update ~update_vars_ordered ~guard ~gt_id ~(probability: OurFloat.t) =
  if com_kind <> "Com_1" then raise OnlyCom1Supported else
    if (OurFloat.(probability > (1. |> of_float)) || OurFloat.(probability < (0. |> of_float))) then raise ProbabilitiesNotBetweenZeroAndOne
  else
  {
    id = get_unique_id id_counter ();
    gt_id;
    input_vars_ordered;
    update;
    update_vars_ordered;
    guard;
    cost = Tuple2.first cvect;
    gtcost = Tuple2.second cvect;
    probability=probability;
    guard_without_invariants = guard; invariants = Guard.mk_true;
  }

(* Generates a nonprobabilistic label and sets the probability to one *)
let make id_counter =
  make_prob id_counter ~gt_id:(get_unique_gt_id id_counter ()) ~probability:OurFloat.one

let update_cost cvect t = {t with cost = Tuple2.first cvect; gtcost = Tuple2.second cvect}

let same lbl1 lbl2 =
  lbl1.id = lbl2.id

let same_gt lbl1 lbl2 =
  lbl1.gt_id = lbl2.gt_id

let equivalent lbl1 lbl2 =
  VarMap.equal UpdateElement.equal lbl1.update lbl2.update
  && Guard.equal lbl1.guard lbl2.guard
  && Polynomial.equal lbl1.cost lbl2.cost
  && OurFloat.equal lbl1.probability lbl2.probability

let compare_same lbl1 lbl2 =
  Int.compare lbl1.id lbl2.id

let compare_equivalent lbl1 lbl2 =
  if VarMap.compare UpdateElement.compare lbl1.update lbl2.update != 0 then
    VarMap.compare UpdateElement.compare lbl1.update lbl2.update
  else if Guard.compare lbl1.guard lbl2.guard != 0 then
    Guard.compare lbl1.guard lbl2.guard
  else if Polynomial.compare lbl1.cost lbl2.cost != 0 then
    Polynomial.compare lbl1.cost lbl2.cost
  else if OurFloat.compare lbl1.probability lbl2.probability != 0 then
    OurFloat.compare lbl1.probability lbl2.probability
  else
    0
let take_last n xs =
  xs
  |> List.rev
  |> List.take n
  |> List.rev

let mk_prob id_counter ?(cvect = (Polynomial.one, RealBound.one)) ~com_kind ~targets ~patterns ~guard ~vars ~gt_id ~probability =
  if List.length targets != 1 then raise RecursionNotSupported else
    if com_kind <> "Com_1" then raise OnlyCom1Supported else
      if (OurFloat.(probability > (1 |> of_int)) || OurFloat.(probability < (0 |> of_int))) then raise ProbabilitiesNotBetweenZeroAndOne
    else
        let (_, assignments) = List.hd targets in

        (* Substitute variables by canonical form, i.e. Arg_0, Arg_1, ...*)
        let arity = List.length patterns in

        (* The substituted input vars, i.e. the arguments to the start location *)
        let input_vars_ordered = LazyList.to_list @@ LazyList.take arity Var.standard_arg_list in

        let substitution_map =
          Enum.combine (List.enum patterns) (List.enum input_vars_ordered)
          |> RenameMap.of_enum
        in

        let substituted_patterns =
          Enum.map (UpdateElement.rename substitution_map) (List.enum assignments)
        in

        let target_vars = LazyList.take (List.length assignments) (Var.standard_arg_list) in

        let update =
          Enum.combine (LazyList.enum target_vars) substituted_patterns
          |> VarMap.of_enum
        in

        {
          probability;
          id = get_unique_id id_counter ();
          gt_id;
          input_vars_ordered;
          update;
          update_vars_ordered = LazyList.to_list target_vars;
          guard = Guard.rename guard substitution_map;
          guard_without_invariants = Guard.rename guard substitution_map;
          invariants = Guard.mk_true;
          cost = Polynomial.rename substitution_map (Tuple2.first cvect);
          gtcost = RealBound.rename substitution_map (Tuple2.second cvect);
        }


let mk id_counter =
  mk_prob id_counter ~gt_id:(get_unique_gt_id id_counter ()) ~probability:OurFloat.one

(*
  Chaining can not be represented in the probabilistic update case due to probability distributions
*)
let append id_counter ~new_gt_id t1 t2 =
  let module VarTable = Hashtbl.Make(Var) in
  let nondet_vars = VarTable.create 3 in
  let substitution update_map var =
    VarMap.Exceptionless.find var update_map
    |? Polynomial.of_var
         (* Variables which are nondeterministic in the preceding transition are represented by fresh variables. *)
         (VarTable.find_option nondet_vars var
          |> Option.default_delayed (fun () ->
                 let nondet_var = Var.fresh_id Var.Int () in
                 VarTable.add nondet_vars var nondet_var;
                 nondet_var
               )
         )
  in
  let get_update_polynomials u_map =
    VarMap.map (function u -> match u with
                                | UpdateElement.Poly p -> p
                                | otherwise -> failwith "This case should be impossible"
               ) u_map
  in

  let subst_t1_update = substitution (get_update_polynomials t1.update) in

  let new_update =
    VarMap.map (UpdateElement.substitute subst_t1_update) t2.update
  and new_guard =
    Guard.Infix.(t1.guard && Guard.map_polynomial (Polynomial.substitute_f (substitution (get_update_polynomials t1.update))) t2.guard)
  and new_guard_without_invariants =
    Guard.Infix.(t1.guard_without_invariants && Guard.map_polynomial (Polynomial.substitute_f (substitution
      (get_update_polynomials t1.update))) t2.guard_without_invariants)
  in

  let t2_cost_subst = Polynomial.substitute_f subst_t1_update t2.cost in
  let t2_gtcost_subst = RealBound.substitute_f (RealBound.of_intpoly % subst_t1_update) t2.gtcost in
  {
    id = get_unique_id id_counter ();
    gt_id = new_gt_id;
    input_vars_ordered = t1.input_vars_ordered;
    update = new_update;
    update_vars_ordered = t2.update_vars_ordered;
    guard = new_guard;
    cost = Polynomial.(t1.cost + t2_cost_subst);
    gtcost = RealBound.(t1.gtcost + t2_gtcost_subst);
    probability = OurFloat.(t1.probability * t2.probability);
    guard_without_invariants = new_guard_without_invariants;
    invariants = t2.invariants;
  }

let append_guard t1 t2 =
  let module VarTable = Hashtbl.Make(Var) in
  let nondet_vars = VarTable.create 3 in
  let dist_vars = VarTable.create 3 in
  let handle_update var u =
    match u with
      | UpdateElement.Poly p -> p
      | UpdateElement.Dist d -> Polynomial.of_var
         (VarTable.find_option dist_vars var
          |> Option.map fst
          |> Option.default_delayed (fun () ->
                 let nondet_var = Var.fresh_id Var.Int () in
                 VarTable.add dist_vars var (nondet_var,d);
                 nondet_var
               ) )
  in
  let substitution (update_map : UpdateElement.t VarMap.t) var =
    (VarMap.Exceptionless.find var update_map
     |> Option.map (handle_update var) )
    |? Polynomial.of_var
         (* Variables which are nondeterministic in the preceding transition are represented by fresh variables. *)
         (VarTable.find_option nondet_vars var
          |> Option.default_delayed (fun () ->
                 let nondet_var = Var.fresh_id Var.Int () in
                 VarTable.add nondet_vars var nondet_var;
                 nondet_var
               )
         )
  in
  let dist_guard =
    VarTable.fold (fun var (var',d) g -> Guard.Infix.(g && (ProbDistribution.guard d var var')) ) dist_vars Guard.mk_true
  in
  let new_guard =
    Guard.Infix.(t1.guard && dist_guard && Guard.map_polynomial (Polynomial.substitute_f (substitution t1.update)) t2.guard)
  in
  new_guard

let id t = t.id
let gt_id t = t.gt_id

let update t var = VarMap.Exceptionless.find var t.update
let update_map t = t.update

let guard t = t.guard
let guard_without_invariants t = t.guard_without_invariants
let invariants t = t.invariants

let add_invariant inv label =
  let new_guard = Guard.mk_and label.guard inv in
  let new_invariants = Guard.mk_and label.invariants inv in
  {label with guard = new_guard; invariants = new_invariants}


let cost t = t.cost

let gtcost t = t.gtcost

let probability t = t.probability

let vars  t =
  VarSet.of_list t.input_vars_ordered
  |> VarSet.union (VarMap.fold (fun v -> VarSet.union % UpdateElement.vars v) t.update VarSet.empty)
  |> VarSet.union (Guard.vars t.guard)
  |> VarSet.union (Polynomial.vars t.cost)
  |> VarSet.union (RealBound.vars t.gtcost)

let default = {
    id = -1;
    gt_id = -1;
    input_vars_ordered = [];
    update = VarMap.empty;
    update_vars_ordered = [];
    guard = Guard.mk_true;
    guard_without_invariants = Guard.mk_true;
    invariants = Guard.mk_true;
    cost = Polynomial.one;
    gtcost = RealBound.one;
    probability = 1. |> OurFloat.of_float
  }

let update_to_string update =
  if VarMap.is_empty update then
    "true"
  else
    update
    |> VarMap.bindings
    |> List.map (fun (var,poly) -> (Var.to_string var, Polynomial.to_string poly))
    |> List.split
    |> fun (xs,ys) -> "("^(String.concat "," xs)^") -> ("^(String.concat "," ys)^")"

let guard_to_string label =
  if
    Guard.is_true label.guard then ""
  else
    Guard.to_string label.guard

let update_to_string_lhs t =
  let i_v_o = t.input_vars_ordered in
  if List.is_empty i_v_o then
    ""
  else
    i_v_o
    |> List.map Var.to_string
    |> fun xs -> "("^(String.concat "," xs)^")"

let update_to_string_rhs t =
  let update = t.update in
    if VarMap.is_empty update then
      ""
    else
      update
      |> VarMap.bindings
      |> List.map (fun (var,poly) -> (Var.to_string var, UpdateElement.to_short_string poly))
      |> List.split
      |> Tuple2.second
      |> fun xs -> "("^(String.concat "," xs)^")"

 let to_string label =
  let guard = if Guard.is_true label.guard then "" else ":|:" ^ Guard.to_string label.guard in
  let cost = if Polynomial.is_one label.cost then "->" else "-{"^ Polynomial.to_string label.cost ^ "}>" ^ ", " in
  let probability = if (label.probability = (1. |> OurFloat.of_float)) then "" else "\n p:" ^ OurFloat.to_string label.probability in
  "ID: " ^ string_of_int label.id ^ ", " ^ "GTID: " ^ string_of_int label.gt_id ^ ", " ^ (update_to_string_lhs label)^ probability ^ cost ^ update_to_string_rhs label ^ guard


let to_id_string t =
  (id t |> string_of_int) ^ "," ^ (gt_id t |> string_of_int)

let input_vars t =
  t.update
  |> VarMap.keys
  |> VarSet.of_enum
(*   VarSet.of_list (t.input_vars_ordered) *)

let input_size t =
  VarSet.cardinal (input_vars t)
