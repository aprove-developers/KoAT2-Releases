open Batteries
open Formulas
open Polynomials
   
let logger = Logger.make_log "lsb"
           
type formula = Formula.t

type template =
  | Equality of int
  | AddsConstant of int
  | ScaledSum of int * int
  | Unbound [@@deriving eq, show]
  
type t = template * VarSet.t

let mk template vars =
  (template, VarSet.of_list (List.map Var.of_string vars))
       
let equal c1 c2 =
  match (c1,c2) with
  | ((cl1, vars1), (cl2, vars2)) -> equal_template cl1 cl2 && VarSet.equal vars1 vars2

let as_bound (template, vars) =
  let var_list = VarSet.map_to_list Bound.of_var vars in
  let open Bound in
  match template with
  | Equality c ->
     maximum (of_int c :: var_list)
  | AddsConstant d ->
     of_int d + maximum var_list
  | ScaledSum (s,e) ->
     of_int s * (of_int e + sum var_list)
  | Unbound -> infinity

(* There is no to_string for options in batteries,
   but there is a very efficient print function which is however a bit inconvenient to use. *)
let to_string_option_template_bound (option: t Option.t): string =
  let output = IO.output_string () in
  Option.print (fun output template_bound -> IO.nwrite output (Bound.to_string (as_bound template_bound))) output option;
  IO.close_out output

let to_string = function
  | (template, vars) -> (show_template template) ^ VarSet.to_string vars
                
let as_formula in_v (template, vars) =
  let var_list = VarSet.map_to_list Polynomial.from_var vars
  and v = Polynomial.from_var in_v in
  let open Polynomial in
  match template with
  | Equality c ->
     Formula.le_than_any v (of_int c :: var_list)
  | AddsConstant d ->
     Formula.le_than_any v (List.map ((+) (of_int d)) var_list)
  | ScaledSum (s,e) ->
     Formula.Infix.(v <= of_int s * (of_int e + sum var_list))
  | Unbound ->
     Formula.mk_true

let is_bounded_with var formula template_bound =
  template_bound
  |> as_formula var
  |> Formula.implies formula
  |> Formula.neg
  |> SMT.Z3Solver.unsatisfiable

(** Performs a binary search between the lowest and highest value to find the optimal value which satisfies the predicate.
    We assume that the highest value already satisfies the predicate.
    Therefore this method always finds a solution. *)
let binary_search (lowest: int) (highest: int) (p: int -> bool) =
  let rec binary_search_ lowest highest p =
    if lowest >= highest then
      highest
    else
      (* We need to ensure that the result is always round down to prevent endless loops.
         Normal integer division rounds towards zero. *)
      let newBound = Float.to_int (Float.floor (Float.div (Float.of_int (lowest + highest)) 2.)) in
      if p newBound then
        binary_search_ lowest newBound p
      else
        binary_search_ (newBound + 1) highest p
  in
  Logger.with_log logger Logger.DEBUG
                  (fun () -> "binary search optimum", ["lowest", Int.to_string lowest; "highest", Int.to_string highest])
                  ~result:Int.to_string
                  (fun () -> binary_search_ lowest highest p)
                                                                                          
(** Minimizes the given variable set, such that the predicate p is still satisfied.
    We assume that the given variable set satisfies the predicate p.
    TODO Currently we use an arbitrary order. This is sound, however for "x <= y && y <= z" we may return z although y would be definitely better. *)
let minimize_vars (vars: VarSet.t) (p: VarSet.t -> bool): VarSet.t =
  let minimize_vars_ vars p =
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
                  (fun () -> "minimize vars", ["vars", VarSet.to_string vars])
                  ~result:(fun result -> VarSet.to_string result)
                  (fun () -> minimize_vars_ vars p)
  
let find_equality_bound vars var formula =
  let find_equality_bound_ vars var formula =
    let low = -1024
    and high = 1024 in
    let is_bound c = is_bounded_with var formula (Equality c, vars) in
       Option.some high
    |> Option.filter is_bound
    |> Option.map (fun high -> binary_search low high is_bound)
    |> Option.filter (fun c -> c != low)
    |> Option.map (fun c ->
      let minimized_vars = minimize_vars vars (fun newVars -> is_bounded_with var formula (Equality c, newVars)) in
      (Equality c, minimized_vars)
    )
  in
  Logger.with_log logger Logger.DEBUG
                  (fun () -> "looking for equality bound", ["vars", VarSet.to_string vars])
                  ~result:(fun result -> to_string_option_template_bound result)
                  (fun () -> find_equality_bound_ vars var formula)

let find_addsconstant_bound vars var formula =
  let find_addsconstant_bound_ var formula =
    let low = -1024
    and high = 1024 in
    let is_bound d = is_bounded_with var formula (AddsConstant d, vars) in
    if is_bound high then
      let d = binary_search low high is_bound in
      let minimized_vars = minimize_vars vars (fun newVars -> is_bounded_with var formula (AddsConstant d, newVars)) in
      Some (AddsConstant d, minimized_vars)
    else None
  in
  Logger.with_log logger Logger.DEBUG
                  (fun () -> "looking for addsconstant bound", [])
                  ~result:(fun result -> to_string_option_template_bound result)
                  (fun () -> find_addsconstant_bound_ var formula)

let find_bound var formula =
  (* Our strategy to find local size bounds.
     Functions which come first have precedence over later functions.
     If none of the functions finds a bound the result is Unbound. *)
  let finders =
    List.enum [
        (* Check if x <= c. We have to check this up front, because find_equality_bound will always prefer a variable with x <= max{-inf,v}. *)
        find_equality_bound VarSet.empty;
        find_equality_bound (VarSet.remove var (Formula.vars formula));
        find_addsconstant_bound (VarSet.remove var (Formula.vars formula));
      ] in
  let find_bound_ var formula =
    try
      Enum.find_map (fun find -> find var formula) finders
    with Not_found -> (Unbound, VarSet.empty) in
  Logger.with_log logger Logger.DEBUG
                  (fun () -> "find local size bound", ["var", Var.to_string var; "formula", Formula.to_string formula])
                  ~result:(fun result -> Bound.to_string (as_bound result))
                  (fun () -> find_bound_ var formula)
                  
let sizebound_local kind label var =
  (* If we have an update pattern, it's like x'=b and therefore x'<=b and x >=b and b is a bound for both kinds. *)
  match TransitionLabel.update label var with
  | Some bound -> (
    (* Introduce a temporary result variable *)
    let v' = Var.fresh_id () in
    let guard_with_update = Formula.Infix.(Formula.mk (TransitionLabel.guard label) && Polynomial.from_var v' = bound) in
    match kind with
    | TransitionLabel.Upper ->
       find_bound v' guard_with_update
    | TransitionLabel.Lower ->
       (* TODO Not yet implemented *)
       (Unbound, VarSet.empty)
  )
  (* If we don't have an update, the result variable is completely unbounded *)
  | None ->
     (Unbound, VarSet.empty)

