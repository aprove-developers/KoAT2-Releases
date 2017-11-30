open Batteries
open Formulas
open Polynomials
   
let logger = Logging.(get LocalSizeBound)
           
type t = {
    kind: [`Lower | `Upper];
    factor: int;
    constant: int;
    vars: ([`Pos | `Neg] * [`Pure | `Abs]) -> VarSet.t
    [@equal fun a b -> List.cartesian_product [`Pos; `Neg] [`Pure; `Abs] |> List.for_all (fun arg -> VarSet.equal (a arg) (b arg))];
  } [@@deriving eq]

let mk ?(s=1) ?(c=0) ?(pos_abs=[]) ?(pos_pure=[]) ?(neg_abs=[]) ?(neg_pure=[]) kind = {
    kind;
    factor = s;
    constant = c;
    vars =
      function
      | (`Pos, `Pure) -> VarSet.of_string_list pos_pure
      | (`Pos, `Abs) -> VarSet.of_string_list pos_abs
      | (`Neg, `Pure) -> VarSet.of_string_list neg_pure
      | (`Neg, `Abs) -> VarSet.of_string_list neg_abs
  }
       
let factor lsb =
  lsb.factor
  
let constant lsb =
  lsb.constant
  
let pos_vars { vars; _ } =
  VarSet.union (vars (`Pos, `Pure)) (vars (`Pos, `Abs))
  
let neg_vars { vars; _ } =
  VarSet.union (vars (`Neg, `Pure)) (vars (`Neg, `Abs))

let pure_vars { vars; _ } =
  VarSet.union (vars (`Pos, `Pure)) (vars (`Neg, `Pure))

let abs_vars { vars; _ } =
  VarSet.union (vars (`Pos, `Abs)) (vars (`Neg, `Abs))

let vars lsb =
  VarSet.union (pos_vars lsb) (neg_vars lsb)

let sign = function
  | _, `Pos, _ -> identity
  | _, `Neg, _ -> Bound.neg

let absifier = function
  | _, _, `Pure -> identity
  | `Upper, `Pos, `Abs -> Bound.(max zero)
  | `Upper, `Neg, `Abs -> Bound.(min zero)
  | `Lower, `Pos, `Abs -> Bound.(min zero)
  | `Lower, `Neg, `Abs -> Bound.(max zero)

let pre_kind = function
  | `Upper, `Pos, _ -> `Upper
  | `Upper, `Neg, _ -> `Lower
  | `Lower, `Pos, _ -> `Lower
  | `Lower, `Neg, _ -> `Upper
  
let sum_vars substitution (vars: VarSet.t) setup: Bound.t =
  vars
  |> VarSet.enum
  |> Enum.map Bound.of_var
  |> Enum.map (absifier setup)
  |> Enum.map (sign setup)
  |> Bound.sum
  |> Bound.substitute_f (substitution (pre_kind setup))
  
let as_substituted_bound substitution lsb =
  let variables =
    List.cartesian_product [`Pos; `Neg] [`Pure; `Abs]
    |> List.map (fun (sign, purifier) -> sum_vars substitution (lsb.vars (sign, purifier)) (lsb.kind, sign, purifier))
    |> List.enum
    |> Bound.sum
  in
  Bound.(of_int lsb.factor * (of_int lsb.constant + variables))

let as_bound =
  as_substituted_bound (fun _ -> Bound.of_var)
  
let default = function
  | `Lower -> Bound.minus_infinity
  | `Upper -> Bound.infinity
  
let to_string lsb =
  String.concat " " ["ScaledSum";
                     String.of_int lsb.factor;
                     String.of_int lsb.constant;
                     VarSet.to_string (lsb.vars (`Pos, `Pure));
                     VarSet.to_string (lsb.vars (`Pos, `Abs));
                     VarSet.to_string (lsb.vars (`Neg, `Pure));
                     VarSet.to_string (lsb.vars (`Neg, `Abs))]

(* [x;y] -> [x+y; x+0; 0+y; 0+0] *)
let abs_vars_combinations vars =
  vars
  |> VarSet.map_to_list Polynomial.of_var
  |> List.map (fun v -> [v; Polynomial.zero])
  |> List.n_cartesian_product  
  |> List.map (Polynomial.sum % List.enum)

let pure_vars_sum lsb =
  let sum_up vars =
    vars
    |> VarSet.map_to_list Polynomial.of_var
    |> List.enum
    |> Polynomial.sum
  in Polynomial.(sum_up (lsb.vars (`Pos, `Pure)) - sum_up (lsb.vars (`Neg, `Pure)))

let as_formula in_v lsb =
  let v = Polynomial.of_var in_v in
  abs_vars_combinations (lsb.vars (`Pos, `Abs))
  |> List.map (fun pos_var_combination ->
         abs_vars_combinations (lsb.vars (`Neg, `Abs))
         |> List.map (fun neg_var_combination ->
                let comp = match lsb.kind with
                  | `Upper -> Formula.Infix.(<=)
                  | `Lower -> Formula.Infix.(>=)
                in Polynomial.(comp v (of_int lsb.factor * (of_int lsb.constant + pure_vars_sum lsb + pos_var_combination - neg_var_combination)))
              )
         |> Formula.any
       )
  |> Formula.any

let is_bounded_with var formula lsb =
  lsb
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

let unabsify var lsb =
  if VarSet.mem var (lsb.vars (`Pos, `Abs)) then
    { lsb with vars = function
                      | (`Pos, `Abs) -> VarSet.remove var (lsb.vars (`Pos, `Abs))
                      | (`Pos, `Pure) -> VarSet.add var (lsb.vars (`Pos, `Pure))
                      | arg -> lsb.vars arg
    }
  else
    { lsb with vars = function
                      | (`Neg, `Abs) -> VarSet.remove var (lsb.vars (`Neg, `Abs))
                      | (`Neg, `Pure) -> VarSet.add var (lsb.vars (`Neg, `Pure))
                      | arg -> lsb.vars arg
    }
 
(** Tries to convert conservatively choosed absolute values of variables by the variables themself. *)
let unabsify_vars (p: t -> bool) (lsb: t): t =
  let execute () =
    let unabsify_with_candidate var current_lsb =
      let possible_better_lsb = unabsify var current_lsb in
      if p possible_better_lsb then
        possible_better_lsb
      else
        current_lsb
    in
    VarSet.fold unabsify_with_candidate (abs_vars lsb) lsb
  in
  Logger.with_log logger Logger.DEBUG
                  (fun () -> "unabsify vars", ["lsb", to_string lsb])
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
                  (fun () -> "minimize vars", ["vars", VarSet.to_string vars])
                  ~result:(fun result -> VarSet.to_string result)
                  execute
  
let minimize_scaledsum_vars (p: t -> bool) (lsb: t): t =
  let minimize_vars sign =
    minimize_vars
      (fun vars ->
        p { lsb with
            vars = function
                   | (s, `Abs) when s = sign -> vars
                   | arg -> lsb.vars arg
          }
      ) (lsb.vars (sign, `Abs))
  in
  (* For Performance *)
  let minimized_pos_vars = minimize_vars `Pos
  and minimized_neg_vars = minimize_vars `Neg in
  { lsb with vars = function
                    (* TODO Maybe we have to do first one sign, then the other instead of do it parallel. *)
                    | (`Pos, `Abs) -> minimized_pos_vars
                    | (`Neg, `Abs) -> minimized_neg_vars
                    | (sign, `Pure) -> lsb.vars (sign, `Pure)
  }

let optimize_c (range: int) (p: t -> bool) (lsb: t): t =
  let execute () = {
      lsb with constant = match lsb.kind with
                          | `Upper -> binary_search (-range) range (fun c -> p { lsb with constant = c } )
                          | `Lower -> - (binary_search (-range) range (fun c -> p { lsb with constant = -c } ))
    }
  in
  Logger.with_log logger Logger.DEBUG
                  (fun () -> "optimize_c", ["lowest", Int.to_string (-range); "highest", Int.to_string range; "lsb", to_string lsb])
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

let initial_lsb kind factor (constant: int) (vars: VarSet.t) = {
    kind;
    factor;
    constant = (
      match kind with
      | `Upper -> constant
      | `Lower -> -constant
    );
    vars = function
           | (_, `Abs) -> vars
           | (_, `Pure) -> VarSet.empty
  }
  
  
(* Check if x <= 1 * (c + [v1,...,vn]). *)
let find_unscaled_bound kind (var: Var.t) (formula: Formula.t) (varsets: VarSet.t Enum.t): t Option.t =
  let execute () =
    try
      Some (
          Enum.find_map (fun vars ->
              Some (initial_lsb kind 1 1024 vars)
              |> Option.filter (is_bounded_with var formula)
              |> Option.map (optimize_c 1024 (is_bounded_with var formula))
              |> Option.map (minimize_scaledsum_vars (is_bounded_with var formula))
              |> Option.map (unabsify_vars (is_bounded_with var formula))
            ) varsets
        )
    with Not_found -> None
  in Logger.with_log logger Logger.DEBUG
                  (fun () -> "find unscaled bound", ["var", Var.to_string var; "formula", Formula.to_string formula])
                  ~result:(Util.option_to_string (Bound.to_string % as_bound))
                  execute
  
(* Check if x <= s * (c + [v1,...,vn]). *)
let find_scaled_bound kind var formula =
  let execute () =
    let vars = VarSet.remove var (Formula.vars formula)
    and range = 1024
    and is_bounded = is_bounded_with var formula in
    try 
      Some (initial_lsb kind range range vars)
      |> Option.filter is_bounded
      |> Option.map (optimize_s 1 range is_bounded)
      |> Option.map (optimize_c range is_bounded)
      |> Option.map (minimize_scaledsum_vars is_bounded)
      |> Option.map (unabsify_vars is_bounded)
    with Not_found -> None
  in Logger.with_log logger Logger.DEBUG
                  (fun () -> "find scaled bound", ["var", Var.to_string var; "formula", Formula.to_string formula])
                  ~result:(Util.option_to_string (Bound.to_string % as_bound))
                  execute

type kind = [`Lower | `Upper] [@@deriving show]
   
let find_bound kind var formula =
  let execute () =
    let unscaled_bound =
      formula
      |> Formula.vars
      |> VarSet.remove var
      |> VarSet.powerset
      |> find_unscaled_bound kind var formula
    in
    if Option.is_some unscaled_bound then
      unscaled_bound
    else
      find_scaled_bound kind var formula
  in Logger.with_log logger Logger.DEBUG
                     (fun () -> "find local size bound", ["kind", show_kind kind; "var", Var.to_string var; "formula", Formula.to_string formula])
                     ~result:(Util.option_to_string (Bound.to_string % as_bound))
                     execute
      
   
let sizebound_local kind label var =
    let open Option.Infix in
    (* If we have an update pattern, it's like x'=b and therefore x'<=b and x >=b and b is a bound for both kinds. *)
    TransitionLabel.update label var
    >>= fun bound ->
    (* Introduce a temporary result variable *)
    let v' = Var.fresh_id () in
    let guard_with_update = Formula.Infix.(Formula.mk (TransitionLabel.guard label) && Polynomial.of_var v' = bound) in
    find_bound kind v' guard_with_update

let sizebound_local_rv kind ((l,t,l'),v) =
  sizebound_local kind t v
