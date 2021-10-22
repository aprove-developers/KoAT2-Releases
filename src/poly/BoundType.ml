open Batteries
open Polynomials
open Util

let logger = Logging.(get Bound)

module Make_BoundOver (Num : PolyTypes.OurNumber)
                      (Poly :
                        sig
                          include PolyTypes.Polynomial with type value = Num.t
                                                        and type valuation = Valuation.Make(Num).t
                                                        and type monomial = Monomials.Make(Num).t
                        end ) =
  struct
    module Valuation_ = Valuation.Make(Num)
    type valuation = Valuation.Make(Num).t

    type value = Num.t

(* Minus Infinity is max of an empty list *)
(* Infinity is min of an empty list *)
type bound =
    Const of Num.t (* absolute value *)
  | Var of Var.t
  | Pow of Num.t * bound
  | Sum of bound * bound
  | Product of bound * bound [@@deriving eq, ord]

type t = bound option [@@deriving eq,ord]
(* None represents infinity *)

let prove_finiteness = identity

let bound_of_var v = Var v
let of_var = OptionMonad.return % bound_of_var

let bound_of_constant c = Const (Num.abs c)
let of_constant = OptionMonad.return % bound_of_constant

let get_constant t =
  let rec get_constant_of_bound b = match b with
    | Const c -> c
    | Var var -> Num.zero
    | Pow (n, b) -> Num.pow (get_constant_of_bound b) (Num.to_int n)
    | Sum (b1, b2) -> Num.add (get_constant_of_bound b1) (get_constant_of_bound b2)
    | Product (b1, b2) -> Num.mul (get_constant_of_bound b1) (get_constant_of_bound b2)
  in
  match t with
  | Some b -> get_constant_of_bound b
  | None   -> Num.zero

let rec fold_bound ~const ~var ~plus ~times ~exp p =
  let fold_ = fold_bound ~const ~var ~plus ~times ~exp in
  match p with
  | Var v -> var v
  | Const c -> const c
  | Pow (value, n) -> exp value (fold_ n)
  | Sum (b1, b2) -> plus (fold_ b1) (fold_ b2)
  | Product (b1, b2) -> times (fold_ b1) (fold_ b2)

let fold ~const ~var ~plus ~times ~exp ~inf = function
  | Some b -> fold_bound ~const ~var ~plus ~times ~exp b
  | None -> inf

type complexity =
  | Inf (** Bound is infinite. *)
  | Polynomial of int (** Bound is in asymptotic class O(n^i) *)
  | Exponential of int [@@deriving eq] (** Bound is in corresponding asymptotic class O(2^2^...^n) where the integer value denotes the amount of powers.*)

let rec show_complexity = function
  | Inf -> "Infinity"
  | Polynomial 0 -> "O(1)"
  | Polynomial 1 -> "O(n)"
  | Polynomial x -> "O(n^" ^ Int.to_string x ^ ")"
  | Exponential 1 -> "O(EXP)"
  | Exponential x -> "O(EXP^" ^ show_complexity (Exponential (x-1)) ^ ")"

let show_complexity_termcomp = function
  | Inf -> "MAYBE"
  | Polynomial 0 -> "WORST_CASE(?, O(1))"
  | Polynomial x -> "WORST_CASE(?, O(n^" ^ Int.to_string x ^ "))"
  | Exponential _ -> "WORST_CASE(?, O(EXP))"


let asymptotic_complexity =
  fold
    ~const:(fun _ -> Polynomial 0)
    ~var:(fun _ -> Polynomial 1)
    ~plus:(fun x y ->
      match (x,y) with
      | (Inf,_) -> Inf
      | (_,Inf) -> Inf
      | (Polynomial x, Polynomial y) -> Polynomial (Int.max x y)
      | (Exponential x, Exponential y) -> Exponential (Int.max x y)
      | (Polynomial x, Exponential y) -> Exponential y
      | (Exponential x, Polynomial y) -> Exponential x
    )
    ~times:(fun x y ->
      match (x,y) with
      | (Inf,_) -> Inf
      | (_,Inf) -> Inf
      | (Polynomial x, Polynomial y) -> Polynomial (Int.add x y)
      | (Exponential x, Exponential y) -> Exponential (Int.max x y)
      | (Polynomial x, Exponential y) -> Exponential y
      | (Exponential x, Polynomial y) -> Exponential x
    )
    ~exp:(fun _ b ->
      match b with
      | Inf -> Inf
      | Polynomial x -> Exponential 1
      | Exponential x -> Exponential (Int.succ x)
    )
    ~inf:Inf

(** Returns true iff. the bound is in complexity class O(n) *)
let is_linear bound =
match (asymptotic_complexity bound) with
  | Polynomial 0 -> true
  | Polynomial 1 -> true
  | _ -> false

let compare_asy b1 b2 =
  match (asymptotic_complexity b1,asymptotic_complexity b2) with
  | (Inf,Inf) -> 0
  | (Inf,_) -> 1
  | (_,Inf) -> -1
  | (Exponential x, Exponential y) -> Int.compare x y
  | (Exponential x, _) -> 1
  | (_, Exponential y) -> -1
  | (Polynomial x, Polynomial y) -> Int.compare x y

let min_asy b1 b2 =
  if compare_asy b1 b2 <= 0 then b1 else b2 

(* let is_linear bound =
  let cplx = asymptotic_complexity bound in
    match cplx with
    | (Polynomial n) -> (n == 1)
    | _ -> false *)

let max_of_occurring_constants bound =
  fold
    ~const:identity
    ~var:(fun _ -> Num.one)
    ~plus:Num.add
    ~times:Num.mul
    ~exp:(fun _ -> raise (Failure "Can not compute max_of_occurring_constants for non-polynomial bounds!"))
    ~inf:(raise (Failure "Can not compute max_of_occurring_constants for non-polynomial bounds!"))
    bound

let rec get_op_chain_and_apply_to_atoms f t b = match (t,b) with
  | (`Sum, Sum (b1,b2))             -> get_op_chain_and_apply_to_atoms f t b1 @ get_op_chain_and_apply_to_atoms f t b2
  | (`Product, Product (b1,b2))     -> get_op_chain_and_apply_to_atoms f t b1 @ get_op_chain_and_apply_to_atoms f t b2
  | (_, b)                          -> [f b]

let rec show_bound_inner ?(pretty=false) = function
  | Var v -> if pretty then Var.to_string_index v else Var.to_string v
  | Const c -> Num.to_string c
  | Pow (v, b) -> Num.to_string v ^ "^(" ^ show_bound_inner ~pretty b ^ ")"
  | Sum (b1, b2) ->
      (* Order terms by degree*)
      let sum_chain = get_op_chain_and_apply_to_atoms identity `Sum b1 @ get_op_chain_and_apply_to_atoms identity `Sum b2 in
      let sorted_chain = List.sort (fun b1 b2 -> compare_asy (OptionMonad.return b2) (OptionMonad.return b1)) sum_chain in
      List.fold_lefti (fun s i b -> if i = 0 then show_bound_inner ~pretty b else s^"+"^show_bound_inner ~pretty b) "" sorted_chain
  | Product (Sum (b1, b2), Sum (b3, b4)) -> "(" ^ show_bound_inner ~pretty (Sum (b1, b2)) ^ ")*(" ^ show_bound_inner ~pretty (Sum (b3, b4)) ^ ")"
  | Product (Sum (b1, b2), b3) -> "(" ^ show_bound_inner ~pretty (Sum (b1, b2)) ^ ")*" ^ show_bound_inner ~pretty b3
  | Product (b1, Sum (b2, b3)) -> show_bound_inner ~pretty b1 ^ "*(" ^ show_bound_inner ~pretty (Sum (b2, b3)) ^ ")"
  | Product (b1, b2) -> show_bound_inner ~pretty b1 ^ "*" ^ show_bound_inner ~pretty b2

let show_bound ?(pretty=false) t =
  match t with
    | Some b -> show_bound_inner ~pretty b
    | None -> "inf"

let show ?(pretty=false) ?(complexity=true) bound =
  let complexity_str =
    if complexity then " {" ^ (show_complexity % asymptotic_complexity) bound ^ "}" else ""
  in
  show_bound ~pretty bound ^ complexity_str

let to_string ?(pretty=false) = show ~complexity:true ~pretty

let gt_bound b1 b2 =
  let execute () =
    match (b1,b2) with
    | (Const c1, Const c2) when Num.Compare.(c1>c2) ->
        Some true
    | _  -> None
  in
  Logger.with_log logger Logger.DEBUG
                  (fun () -> ">", ["condition", to_string (Some b1) ^ ">" ^ to_string (Some b2)])
                  ~result:(Util.option_to_string Bool.to_string)
                  execute

let (>) b1 b2 = match (b1,b2) with
  | (Some b1, Some b2) -> gt_bound b1 b2
  | (None, Some _) -> Some true
  | (_, None) -> Some false

let (>=) b1 b2 =
  let execute () =
    match (b1,b2) with
    | (None, None) -> Some true
    | (Some b1, Some b2) -> if equal_bound b1 b2 then Some true else (match (b1,b2) with
        | (Const c1, Const c2) when Num.Compare.(c1 >= c2) ->
            Some true
        | _ -> None)
    | _ -> Some false
  in
  Logger.with_log logger Logger.DEBUG
                  (fun () -> ">=", ["condition", to_string b1 ^ ">=" ^ to_string b2])
                  ~result:(Util.option_to_string Bool.to_string)
                  execute

let (<) = flip (>)

let (<=) = flip (>=)

let (=~=) = equal

(* In the following we simplify (finite) bounds to normal forms.
 * A bound b is in normal form if it can be represented as the sum of products of coefficients and variables, or terms n^b where b
 * is in normal form.
 * E.g. (x+y)*(x+y) should be simplified to x^2 + 2*x*y + y^2
 *)
let rec
  get_op_chain t = get_op_chain_and_apply_to_atoms simplify_bound t
  (* Reverse to get_op_chain, e.g. construct a chain *)
  and construct_op_chain t bs =
    (* Sort terms to allow for better equality checking of similar terms. String comparison is kind of arbitrary *)
    (* We sort in reverse order, and then apply in reverse order again.
     * This has the following advantage:
       Consider a product chain [Var X, Const 1, Var Y]
       Then, by sorting in reverse we get [Var Y, Var X, Const 1].
       By, again applying in reverse we obtain Product (Var X, Var Y) -> Product (Const 1, Product (Var X, Var Y)).
       The coefficient up-front then allows for better simplification later-on
     * *)
    let sorted = List.sort (fun b1 b2 -> String.compare (show_bound_inner b2) (show_bound_inner b1)) bs in
    match t with
      | `Sum ->
          (try List.reduce (fun b1 b2 -> Sum (b2,b1)) sorted
          with Invalid_argument _  -> bound_of_constant (Num.zero))
      | `Product ->
          (try List.reduce (fun b1 b2 -> Product (b2,b1)) sorted
          with Invalid_argument _  -> bound_of_constant (Num.one))

  and simplify_bound bound =
    let execute () =
      match bound with

      | Var v -> Var v

      | Const c -> Const c

      (* Simplify terms with sum head *)
      | Sum (b1, b2) -> (
          let sum_chain =
            get_op_chain `Sum b1 @ get_op_chain `Sum b2
            |> List.filter (not % equal_bound (Const Num.zero))
          in
          (* Merge addends that are a product of the same bound with different coefficients *)
          let combine_chain_elements_with_coeffs =
            let get_coeff_elem = function
              | Product (Const c, b) -> (b,c)
              | Product (b, Const c) -> (b,c)
              | Const k              -> (Const Num.one, k)
              | b                    -> (b,Num.one)
            in
            sum_chain
            |> List.map get_coeff_elem
            |> List.fold_left
                (fun l (b,c) ->
                  try
                    let i = fst @@ List.findi (fun i -> equal_bound b % fst) l in
                    List.modify_at i (fun (b,c') -> (b,Num.(c + c'))) l
                  with Not_found -> List.cons (b,c) l)
                []
            |> List.map (fun (b,c) -> simplify_bound @@ Product (Const c, b))
          in
          (* Finally take the chain with possibly merged addends and construct a bound before applying the 'old' approach to it *)
          construct_op_chain `Sum combine_chain_elements_with_coeffs
        )

      (* Simplify terms with product head *)
      | Product (b1, b2) -> (
          let chain = get_op_chain `Product b1 @ get_op_chain `Product b2 in

          (* Partition the chain in sets of constants and non-constant factors *)
          let get_const = function
            | Const c       -> Some c
            | _             -> None
          in
          let all_non_consts = List.filter (Option.is_none % get_const) chain in
          let all_consts = List.map Option.get @@ List.filter Option.is_some @@ List.map get_const chain in

          (* Get the coefficient of the complete chain by multiplying all of its constant values *)
          let const = List.fold_left Num.mul Num.one all_consts in

          (* Here we have to multiply through *)
          (* let non_const_chain = construct_op_chain `Product all_non_consts in *)

          let final_sum_chain multiply_with_const =
            (* Here we expect all_non_consts to be sums or vars, or exponential terms.
             * For all sums we have to multiply the corresponding chains *)
            List.map (get_op_chain `Sum) all_non_consts
            (* multiply through *)
            |> List.n_cartesian_product
            |> List.map (fun l -> if multiply_with_const then (Const const)::l else l)
            |> List.map (construct_op_chain `Product)
            |> fun l -> (if Int.(List.length l > 1) then simplify_bound (construct_op_chain `Sum l) else construct_op_chain `Sum l)
          in

          if Num.(equal const zero) then
            Const const
          else if Num.(equal const one) then
            final_sum_chain false
          else if List.is_empty all_non_consts then
            Const const
          else
            final_sum_chain true
      )

      (* Simplify terms with pow head *)
      | Pow (value, exponent) -> (
         match simplify_bound exponent with
         | exponent when Num.(equal value zero) && (gt_bound exponent (Const (Num.zero)) |? false) -> Const Num.zero
         | _ when Num.(equal value one) -> Const Num.one
         | Const c -> Const Num.(pow value (to_int c)) (* TODO Do not use Num.to_int *)
         | exponent -> Pow (value, exponent)
      )

    in
    Logger.with_log logger Logger.DEBUG
                    (fun () -> "simplify_bound", ["input", to_string (Some bound)])
                    ~result:(to_string % OptionMonad.return)
                    execute

let simplify = Option.map simplify_bound

let zero_bound = Const (Num.zero)
let zero = Some zero_bound

let one_bound = Const (Num.one)
let one = Some one_bound

let add_bound b1 b2 = simplify_bound (Sum (b1,b2))
let add =
  OptionMonad.liftM2 add_bound

let mul_bound b1 b2 = simplify_bound (Product (b1,b2))
let mul b1 b2 = match (b1, b2) with
  (* 0 * inf = inf * 0 = 0 *)
  | (Some (Const c), None) -> if Num.equal c Num.zero then zero else None
  | (None, Some (Const c)) -> if Num.equal c Num.zero then zero else None
  | _ -> OptionMonad.liftM2 mul_bound b1 b2

let pow_bound bound n =
  bound
  |> Enum.repeat ~times:n
  |> Enum.fold mul_bound one_bound
  |> simplify_bound

let pow bound n =
  Option.map (flip pow_bound n) bound

(** Addition of two elements. *)
let (+) = add

(** Multiplication of two elements *)
let ( * ) = mul

(** Raises an element to the power of an integer value. *)
let ( ** ) = pow

let sum bounds =
  try
    bounds |> Enum.reduce add |> simplify
  with Not_found -> zero

let sum_list = sum % List.enum

let product bounds =
  try
    bounds |> Enum.reduce mul |> simplify
  with Not_found -> one

let of_poly =
  OptionMonad.return %  Poly.fold ~const:bound_of_constant ~var:bound_of_var ~neg:identity ~plus:add_bound ~times:mul_bound ~pow:pow_bound

let bound_of_int i = Const (Num.of_int @@ Int.abs i)
let of_int = OptionMonad.return % bound_of_int

let to_int poly = raise (Failure "TODO: Not possible")

let bound_of_var_string str = Var (Var.of_string str)
let of_var_string = OptionMonad.return % bound_of_var_string

let infinity = None

let is_infinity = Option.is_none
let is_finite = Option.is_some

let exp_bound value b = simplify_bound (Pow (Num.abs value, b))
let exp value b = Option.map (exp_bound value) b

let substitute_f (substitution: Var.t -> t) (bound: t): t =
  OptionMonad.(bound >>= fold_bound ~const:of_constant ~var:substitution ~plus:add ~times:mul ~exp:exp)

let substitute_bound var ~replacement =
  substitute_f (fun target_var ->
      if Var.(var =~= target_var) then replacement else of_var target_var
    )

let substitute var ~replacement b = substitute_bound var ~replacement b

let substitute_all substitution =
  let module VarMap = Map.Make(Var) in
  substitute_f (fun var -> VarMap.find_default (of_var var) var substitution)

let rec vars_bound = function
  | Var v -> VarSet.singleton v
  | Const _ -> VarSet.empty
  | Pow (v,b) -> vars_bound b
  | Sum (b1, b2) -> VarSet.union (vars_bound b1) (vars_bound b2)
  | Product (b1, b2) -> VarSet.union (vars_bound b1) (vars_bound b2)

let vars = Option.default VarSet.empty % Option.map vars_bound

let keep_simpler_bound b1 b2 = match compare_asy b1 b2 with
  (* First compare asymptotic_complexity *)
  | -1 -> b1
  |  1 -> b2
  |  _ ->
      (* Now compare number of variables*)
      match Int.compare (VarSet.cardinal @@ vars b1) (VarSet.cardinal @@ vars b2) with
      | -1 -> b1
      |  1 -> b2
      |  _ ->
          (* Finally compare length of to_string *)
          match Int.compare (String.length @@ show_bound b1) (String.length @@ show_bound b2) with
          | -1 -> b1
          |  1 -> b2
          |  _ -> b1


let degree n = raise (Failure "degree for MinMaxPolynomial not yet implemented")
let rename map p = raise (Failure "rename for MinMaxPolynomial not yet implemented")
let eval p valuation = raise (Failure "eval for MinMaxPolynomial not yet implemented")
let eval_f p valuation = raise (Failure "eval_f for MinMaxPolynomial not yet implemented")
let coeff_of_var p = raise (Failure "coeff_of_var for MinMaxPolynomial not yet implemented")
let of_coeff_list p = raise (Failure "of_coeff_list for MinMaxPolynomial not yet implemented")
end
