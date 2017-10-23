open Batteries
open Formulas
open Polynomials
   
let logger = Logger.make_log "lsb"
           
type formula = Formula.t

type t = ScaledSum of int * int * VarSet.t
  
let mk s e vars =
  ScaledSum (s, e, VarSet.of_string_list vars)
       
let equal c1 c2 =
  match (c1,c2) with
  | (ScaledSum (s1, e1, vars1), ScaledSum (s2, e2, vars2)) ->
     s1 = s2 && e1 = e2 && VarSet.equal vars1 vars2

let as_bound = function
  | Some (ScaledSum (s,e,vars)) ->
     let var_list = VarSet.map_to_list Bound.of_var vars in
     Bound.(of_int s * (of_int e + sum var_list))
  | None -> Bound.infinity

(* There is no to_string for options in batteries,
   but there is a very efficient print function which is however a bit inconvenient to use. *)
let to_string_option_template_bound (option: t Option.t): string =
  let output = IO.output_string () in
  Option.print (fun output template_bound -> IO.nwrite output (Bound.to_string (as_bound (Some template_bound)))) output option;
  IO.close_out output

let to_string = function
  | (ScaledSum (s, e, vars)) -> String.concat " " ["ScaledSum"; String.of_int s; String.of_int e; VarSet.to_string vars]
                
let as_formula in_v = function
  | ScaledSum (s, e, vars) ->
     let v = Polynomial.of_var in_v in
     let var_list = VarSet.map_to_list Polynomial.of_var vars in
     Polynomial.(Formula.Infix.(v <= of_int s * (of_int e + sum var_list)))

let is_bounded_with var formula template_bound =
  template_bound
  |> as_formula var
  |> Formula.implies formula
  |> Formula.neg
  |> (fun f -> Logger.log logger Logger.DEBUG (fun () -> "is_bounded_with", ["formula", Formula.to_string f]); f)
  |> SMT.Z3Solver.unsatisfiable

let is_bounded_with_constant var formula c =
  is_bounded_with var formula (ScaledSum (1, c, VarSet.empty))

let is_bounded_with_var var formula var =
  is_bounded_with var formula (ScaledSum (1, 0, VarSet.singleton var))

let is_bounded_with_var_plus_constant var formula var c =
  is_bounded_with var formula (ScaledSum (1, c, VarSet.singleton var))
  
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
let minimize_vars (p: VarSet.t -> bool) (vars: VarSet.t): VarSet.t =
  let minimize_vars_ () =
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
                  (fun () -> minimize_vars_ ())

let minimize_scaledsum_vars (p: t -> bool) (template_bound: t): t =
  match template_bound with
  | (ScaledSum (s, e, vars)) ->
     ScaledSum (s, e, minimize_vars (fun vars -> p (ScaledSum (s, e, vars))) vars)

let optimize_c (lowest: int) (highest: int) (p: t -> bool) (template_bound: t): t =
  let optimize_c_ () = match template_bound with
  | (ScaledSum (s, e, vars)) ->
     ScaledSum (s, binary_search lowest highest (fun c -> p (ScaledSum (s, c, vars))), vars)
  in
  Logger.with_log logger Logger.DEBUG
                  (fun () -> "optimize_c", ["lowest", Int.to_string lowest; "highest", Int.to_string highest; "template_bound", to_string template_bound])
                  ~result:to_string
                  (fun () -> optimize_c_ ())

let optimize_s (lowest: int) (highest: int) (p: t -> bool) (template_bound: t): t =
  let optimize_s_ () = match template_bound with
    | (ScaledSum (s, e, vars)) ->
       ScaledSum (binary_search lowest highest (fun s -> p (ScaledSum (s, e, vars))), e, vars)
  in
  Logger.with_log logger Logger.DEBUG
                  (fun () -> "optimize_s", ["lowest", Int.to_string lowest; "highest", Int.to_string highest; "template_bound", to_string template_bound])
                  ~result:to_string
                  (fun () -> optimize_s_ ())

let find_unscaled (var: Var.t) (formula: Formula.t) (varsets: VarSet.t Enum.t): t Option.t =
  try
    Some (
        Enum.find_map (fun varset ->
            Some (ScaledSum (1, 1024, varset))
            |> Option.filter (is_bounded_with var formula)
            |> Option.map (optimize_c (-1024) 1024 (is_bounded_with var formula))
          ) varsets
      )
  with Not_found -> None

  
let find_bound var formula =
  (* Our strategy to find local size bounds.
     Functions which come first have precedence over later functions.
     If none of the functions finds a bound the result is Unbound. *)
  (* Check if x <= 1 * (c + [v1,...,vn]). *)
  (* Check if x <= s * (c + [v1,...,vn]). *)
  (* find_equality_bound (VarSet.remove var (Formula.vars formula)); *)
  let find_bound_ () =
    let vars = VarSet.remove var (Formula.vars formula)
    and low = -1024
    and high = 1024 in
    try 
      Some (
          Enum.find_map
            (fun f -> f ())
            (List.enum
               [
                 (fun () -> find_unscaled var formula (VarSet.powerset vars));
                 (fun () -> 
                   Some (ScaledSum (high, high, vars))
                   |> Option.filter (is_bounded_with var formula)
                   |> Option.map (optimize_s 1 high (is_bounded_with var formula))
                   |> Option.map (optimize_c low high (is_bounded_with var formula))
                   |> Option.map (minimize_scaledsum_vars (is_bounded_with var formula))
                 )
               ]
            )
        )
    with Not_found -> None
  in Logger.with_log logger Logger.DEBUG
                  (fun () -> "find local size bound", ["var", Var.to_string var; "formula", Formula.to_string formula])
                  ~result:(fun result -> to_string_option_template_bound result)
                  (fun () -> find_bound_ ())
                  
let sizebound_local kind label var =
  let open Option.Infix in
  (* If we have an update pattern, it's like x'=b and therefore x'<=b and x >=b and b is a bound for both kinds. *)
  TransitionLabel.update label var
  >>= fun bound ->
    (* Introduce a temporary result variable *)
    let v' = Var.fresh_id () in
    let guard_with_update = Formula.Infix.(Formula.mk (TransitionLabel.guard label) && Polynomial.of_var v' = bound) in
    match kind with
    | `Upper ->
       find_bound v' guard_with_update
    | `Lower ->
       find_bound v' (Formula.turn guard_with_update)

let sizebound_local_rv kind ((l,t,l'),v) =
  sizebound_local kind t v
