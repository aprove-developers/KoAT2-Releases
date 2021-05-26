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
                          val max_of_occurring_constants : t -> Num.t
                        end ) =
  struct
    module Valuation_ = Valuation.Make(Num)
    type valuation = Valuation.Make(Num).t

    type polynomial = Poly.t
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


module Constructor =
  struct
    let number = function
      | Const _ -> 1
      | Var _ -> 2
      | Pow _ -> 4
      | Sum _ -> 5
      | Product _ -> 6

    let (<) b1 b2 =
      number b1 < number b2
  end

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

let is_constant bound =
match (asymptotic_complexity bound) with
  | Polynomial 0 -> true
  | _ -> false

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


let show_bound t =
  let rec show_inner = function
    | Var v -> Var.to_string v
    | Const c -> if Num.Compare.(c < Num.zero) then "("^Num.to_string c^")" else Num.to_string c
    | Pow (v, b) -> Num.to_string v ^ "^(" ^ show_inner b ^ ")"
    | Sum (b1, Const b2) when Num.Compare.(b2 < Num.zero) -> show_inner b1 ^ "-" ^ show_inner (Const (Num.neg b2))
    | Sum (b1, b2) -> show_inner b1 ^ "+" ^ show_inner b2
    | Product (Sum (b1, b2), Sum (b3, b4)) -> "(" ^ show_inner (Sum (b1, b2)) ^ ")*(" ^ show_inner (Sum (b3, b4)) ^ ")"
    | Product (Sum (b1, b2), b3) -> "(" ^ show_inner (Sum (b1, b2)) ^ ")*" ^ show_inner b3
    | Product (b1, Sum (b2, b3)) -> show_inner b1 ^ "*(" ^ show_inner (Sum (b2, b3)) ^ ")"
    | Product (b1, b2) -> show_inner b1 ^ "*" ^ show_inner b2
  in
  match t with
    | Some b -> show_inner b
    | None -> "inf"

let show ?(complexity=true) bound =
  let complexity_str =
    if complexity then " {" ^ (show_complexity % asymptotic_complexity) bound ^ "}" else ""
  in
  show_bound bound ^ complexity_str

let to_string = show ~complexity:true

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

let rec (>=) b1 b2 =
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

let rec simplify_bound bound =
  let execute () =
    match bound with

    | Var v -> Var v

    | Const c -> Const c

    (* Simplify terms with sum head *)
    | Sum (b1, b2) -> (
      match (simplify_bound b1, simplify_bound b2) with
      | (Const c, b) when Num.(c =~= zero) -> b
      | (b, Const c) when Num.(c =~= zero) -> b
      | (Const c1, Const c2) -> Const Num.(c1 + c2)
      | (Const c1, Sum (Const c2, b)) -> simplify_bound (Sum (Const Num.(c1 + c2), b))
      | (b1, b2) when equal_bound b1 b2 -> simplify_bound (Product (Const (Num.of_int 2), b1))
      | (b1, Sum (b2, b3)) when Constructor.(b2 < b1) -> simplify_bound (Sum (b2, Sum (b1, b3)))
      | (Sum (b1, b2), b3) when Constructor.(b3 < b2) -> simplify_bound (Sum (Sum (b1, b3), b2))
      | (b1, b2) when Constructor.(b2 < b1) -> simplify_bound (Sum (b2, b1))
      | (b1, b2) -> Sum (b1, b2)
    )

    (* Simplify terms with product head *)
    | Product (b1, b2) -> (
      match (simplify_bound b1, simplify_bound b2) with
      | (Const c1, Const c2) -> Const (Num.(c1 * c2))
      | (Const c, b) when Num.(c =~= one) -> b
      | (b, Const c) when Num.(c =~= one) -> b
      | (Const c, b) when Num.(c =~= zero) -> Const Num.zero
      | (b, Const c) when Num.(c =~= zero) -> Const Num.zero
      | (b1, Product (b2, b3)) when Constructor.(b2 < b1) -> simplify_bound (Product (b2, Product (b1, b3)))
      | (Product (b1, b2), b3) when Constructor.(b3 < b2) -> simplify_bound (Product (Product (b1, b3), b2))
      | (b1, b2) when Constructor.(b2 < b1) -> simplify_bound (Product (b2, b1))
      | (b1, b2) -> Product (b1, b2)
    )

    (* Simplify terms with pow head *)
    | Pow (value, exponent) -> (
       match simplify_bound exponent with
       | exponent when Num.(equal value zero) && (gt_bound exponent (Const (Num.zero)) |? false) -> Const Num.zero
       | _ when Num.(equal value one) -> Const Num.one
       | Const c -> Const Num.(pow value (to_int c))
       (* TODO Do not use Num.to_int *)
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
let mul =
  OptionMonad.liftM2 mul_bound

let pow_bound bound n =
  bound
  |> Enum.repeat ~times:n
  |> Enum.fold mul_bound one_bound
  |> simplify_bound

let pow bound n =
  Option.map (flip pow_bound n) bound

(** Returns the sum of all enums elements. *)
let sum = Enum.fold add zero

(** Returns the product of all enums elements. *)
let product = Enum.fold mul one

(** Addition of two elements. *)
let (+) = add

(** Multiplication of two elements *)
let ( * ) = mul

(** Raises an element to the power of an integer value. *)
let ( ** ) = pow

(** Negates element. *)
let (~-) = neg

let sum bounds =
  try
    bounds |> Enum.reduce add |> simplify
  with Not_found -> zero

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

let maximum bounds =
  try
    bounds |> Enum.reduce max |> simplify
  with Not_found -> zero

let minimum bounds =
  try
    bounds |> Enum.reduce min |> simplify
  with Not_found -> infinity

let exp_bound value b = simplify_bound (Pow (Num.abs value, b))
let exp value b = Option.map (exp_bound value) b

let is_var = function
  | Var _ -> true
  | _ -> false

let substitute_f_bound substitution bound =
  Option.map (simplify_bound % fold_bound ~const:bound_of_constant ~var:substitution ~plus:add_bound ~times:mul_bound ~exp:exp_bound) bound

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

let contains_infinity =
  Option.is_none

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
let of_string p = raise (Failure "of_string for MinMaxPolynomial not yet implemented")
let coeff_of_var p = raise (Failure "coeff_of_var for MinMaxPolynomial not yet implemented")
let of_coeff_list p = raise (Failure "of_coeff_list for MinMaxPolynomial not yet implemented")
end
