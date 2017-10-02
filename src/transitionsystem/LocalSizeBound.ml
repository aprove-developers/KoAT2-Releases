open Batteries

let logger = Logger.make_log "lsb"
           
module Formula = Formula.PolynomialFormula
type formula = Formula.t

module VarSet = Set.Make(Var)
type varset = Set.Make(Var).t
            
type template =
  | Equality of int
  | AddsConstant of int
  | ScaledSum of int * int
  | Unbound [@@deriving eq, show]
  
type t = template * varset

let mk template vars =
  (template, VarSet.of_list (List.map Var.of_string vars))
       
let equal c1 c2 =
  match (c1,c2) with
  | ((cl1, vars1), (cl2, vars2)) -> equal_template cl1 cl2 && VarSet.equal vars1 vars2

let as_bound (template, vars) =
  let var_list = List.map Bound.of_var (VarSet.to_list vars) in
  let open Bound in
  match template with
  | Equality c ->
     maximum (of_int c :: var_list)
  | AddsConstant d ->
     of_int d + maximum var_list
  | ScaledSum (s,e) ->
     of_int s * (of_int e + sum var_list)
  | Unbound -> infinity

(* There is no to_string for sets in batteries,
   but there is a very efficient print function which is however a bit inconvenient to use. *)
let to_string_varset (vars: varset): string =
  let output = IO.output_string () in
  VarSet.print (fun output var -> IO.nwrite output (Var.to_string var)) output vars;
  IO.close_out output
       
(* There is no to_string for options in batteries,
   but there is a very efficient print function which is however a bit inconvenient to use. *)
let to_string_option_template_bound (option: t Option.t): string =
  let output = IO.output_string () in
  Option.print (fun output template_bound -> IO.nwrite output (Bound.to_string (as_bound template_bound))) output option;
  IO.close_out output

let to_string = function
  | (template, vars) -> (show_template template) ^ to_string_varset vars
                
let as_formula in_v (template, vars) =
  let var_list = List.map Polynomial.from_var (VarSet.to_list vars)
  and v = Polynomial.from_var in_v in
  let open Polynomial in
  match template with
  | Equality c ->
     Formula.mk_le_than_max v (of_int c :: var_list)
  | AddsConstant d ->
     Formula.mk_le_than_max v (List.map ((+) (of_int d)) var_list)
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
                                                                                          
(** Minimizes the given variable set, such that the predicate p is still satisified.
    We assume that the given variable set satisfies the predicate p. *)
let minimize_vars (vars: varset) (p: varset -> bool): varset =
  let minimize_vars_ vars p =
    let is_necessary var = not (p (VarSet.remove var vars)) in
    VarSet.filter is_necessary vars
  in
  Logger.with_log logger Logger.DEBUG
                  (fun () -> "minimize vars", ["vars", to_string_varset vars])
                  ~result:(fun result -> to_string_varset result)
                  (fun () -> minimize_vars_ vars p)
  
let find_equality_bound vars var formula =
  let find_equality_bound_ vars var formula =
    let low = -1024
    and high = 1024 in
    let is_bound c = is_bounded_with var formula (Equality c, vars) in
    if is_bound high then
      (* TODO If the var is bounded for every constant, we can remove the constant. This is the case e.g. if x'=y *)
      let c = binary_search low high is_bound in
      let minimized_vars = minimize_vars vars (fun newVars -> is_bounded_with var formula (Equality c, newVars)) in
      Some (Equality c, minimized_vars)
    else None
  in
  Logger.with_log logger Logger.DEBUG
                  (fun () -> "looking for equality bound", ["vars", to_string_varset vars])
                  ~result:(fun result -> to_string_option_template_bound result)
                  (fun () -> find_equality_bound_ vars var formula)

let find_addsconstant_bound vars var formula =
  let find_addsconstant_bound_ var formula =
    let low = 0
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
  (* TODO Should we also try to substitute vars in the bound if it leads to a simpler bound? E.g. x<=10 && x'=x : b:=x or b:=10? *)
  match TransitionLabel.update label var with
  (* TODO Wrong *)
  | Some bound -> Bound.of_poly bound
  | None ->
     match kind with
     | TransitionLabel.Upper ->
           label
        |> TransitionLabel.guard
        |> Formula.mk
        |> find_bound var
        |> as_bound
     | TransitionLabel.Lower ->
        Bound.minus_infinity

