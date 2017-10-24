open Batteries
open Formulas
open Polynomials
   
let logger = Logger.make_log "lsb"
           
type formula = Formula.t

type t = {
    factor: int;
    constant: int;
    vars: VarSet.t;
  }
  
let mk factor constant vars = {
    factor;
    constant;
    vars = VarSet.of_string_list vars
  }
       
let equal lsb1 lsb2 =
  lsb1.factor = lsb2.factor
  && lsb1.constant = lsb2.constant
  && VarSet.equal lsb1.vars lsb2.vars

let neg lsb =
  { lsb with factor = -lsb.factor }

let abs_factor lsb =
  abs lsb.factor
  
let as_bound = function
  | Some lsb ->
     lsb.vars
     |> VarSet.map_to_list Bound.of_abs_var
     |> fun var_list -> Bound.(of_int lsb.factor * (of_int lsb.constant + sum var_list))
  | None -> Bound.infinity

(* There is no to_string for options in batteries,
   but there is a very efficient print function which is however a bit inconvenient to use. *)
let to_string_option_template_bound (option: t Option.t): string =
  let output = IO.output_string () in
  Option.print (fun output template_bound -> IO.nwrite output (Bound.to_string (as_bound (Some template_bound)))) output option;
  IO.close_out output

let to_string lsb =
  String.concat " " ["ScaledSum"; String.of_int lsb.factor; String.of_int lsb.constant; VarSet.to_string lsb.vars]
                
let as_formula in_v lsb = 
  VarSet.to_list lsb.vars
  |> List.map Polynomial.of_var
  |> List.map (fun v -> [v; Polynomial.neg v])
  |> List.n_cartesian_product
  |> List.map (fun var_list ->
         let v = Polynomial.of_var in_v in
         Polynomial.(Formula.Infix.(v <= of_int lsb.factor * (of_int lsb.constant + sum var_list))))
  |> Formula.any

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

let minimize_scaledsum_vars (p: t -> bool) (lsb: t): t = {
    lsb with vars = minimize_vars
                      (fun vars -> p { lsb with vars = vars } )
                      lsb.vars
  }

let optimize_c (lowest: int) (highest: int) (p: t -> bool) (lsb: t): t =
  let execute () = {
      lsb with constant = binary_search
                            lowest
                            highest
                            (fun c -> p { lsb with constant = c } )
    }
  in
  Logger.with_log logger Logger.DEBUG
                  (fun () -> "optimize_c", ["lowest", Int.to_string lowest; "highest", Int.to_string highest; "lsb", to_string lsb])
                  ~result:to_string
                  execute

let optimize_s (lowest: int) (highest: int) (p: t -> bool) (lsb: t): t =
  let execute () = {
      lsb with factor = binary_search
                          lowest
                          highest
                          (fun s -> p { lsb with factor = s } )
    }
  in
  Logger.with_log logger Logger.DEBUG
                  (fun () -> "optimize_s", ["lowest", Int.to_string lowest; "highest", Int.to_string highest; "lsb", to_string lsb])
                  ~result:to_string
                  execute

let find_unscaled (var: Var.t) (formula: Formula.t) (varsets: VarSet.t Enum.t): t Option.t =
  try
    Some (
        Enum.find_map (fun varset ->
            Some {factor = 1; constant = 1024; vars = varset}
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
  let execute () =
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
                   Some {factor = high; constant = high; vars}
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
                  execute
                  
let sizebound_local kind label var =
  (* TODO Cache let f (kind, label, var) = *)
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
       guard_with_update
       |> Formula.turn
       |> find_bound v'
       |> Option.map neg
  (* TODO Cache, but with care
     in let memo = Lru.memo ~hashed:(Hashtbl.hash, fun (kind1, label1, var1) (kind2, label2, var2) -> kind1 = kind2 && TransitionLabel.equal label1 label2 && Var.equal var1 var2)
     ~cap:50 (fun _ -> f) in
     memo (kind, label, var)
   *)
      
let sizebound_local_rv kind ((l,t,l'),v) =
  sizebound_local kind t v
