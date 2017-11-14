open Batteries
open Polynomials
open Util
   
let logger = Logging.(get Bound)
           
module Valuation_ = Valuation.Make(OurInt)
type valuation = Valuation.Make(OurInt).t
                  
type polynomial = Polynomial.t
type value = OurInt.t
                
(* Minus Infinity is max of an empty list *)
(* Infinity is min of an empty list *)
type t =
  | Infinity
  | Const of OurInt.t
  | Var of Var.t
  | Abs of t
  | Neg of t
  | Pow of OurInt.t * t
  | Sum of t * t
  | Product of t * t 
  | Max of t * t
  | Min of t * t [@@deriving eq, ord]

let of_var v = Var v

let of_constant c = Const c

module Constructor =
  struct
    let number = function
      | Infinity -> 0
      | Const _ -> 1
      | Var _ -> 2
      | Abs _ -> 3
      | Neg _ -> 4
      | Pow _ -> 5
      | Sum _ -> 6
      | Product _ -> 7
      | Max _ -> 8
      | Min _ -> 9
        
    let (<) b1 b2 =
      number b1 < number b2
  end
          
let rec to_string = function
  | Var v -> Var.to_string v
  | Abs b -> "|" ^ to_string b  ^ "|"
  | Const c -> OurInt.to_string c
  | Infinity -> "inf"
  | Max (b1, b2) -> "max{" ^ to_string b1 ^ ", " ^ to_string b2 ^ "}"
  | Min (b1, b2) -> "min{" ^ to_string b1 ^ ", " ^ to_string b2 ^ "}"
  | Neg b -> "-" ^ (
      match b with
      | Sum (b1, b2) -> "(" ^ to_string (Sum (b1, b2)) ^ ")"
      | Product (b1, b2) -> "(" ^ to_string (Product (b1, b2)) ^ ")"
      | b -> to_string b
    )
  | Pow (v, b) -> OurInt.to_string v ^ "^(" ^ to_string b ^ ")"
  | Sum (b1, Neg b2) -> to_string b1 ^ "-" ^ to_string b2
  | Sum (b1, Const b2) when OurInt.Compare.(b2 < OurInt.zero) -> to_string b1 ^ "-" ^ to_string (Const (OurInt.neg b2))
  | Sum (b1, b2) -> to_string b1 ^ "+" ^ to_string b2
  | Product (Sum (b1, b2), Sum (b3, b4)) -> "(" ^ to_string (Sum (b1, b2)) ^ ")*(" ^ to_string (Sum (b3, b4)) ^ ")"
  | Product (Sum (b1, b2), b3) -> "(" ^ to_string (Sum (b1, b2)) ^ ")*" ^ to_string b3
  | Product (b1, Sum (b2, b3)) -> to_string b1 ^ "*(" ^ to_string (Sum (b2, b3)) ^ ")"
  | Product (b1, b2) -> to_string b1 ^ "*" ^ to_string b2

let rec (>) b1 b2 =
  let execute () =
    match (b1, b2) with
    | (_, Infinity) -> Some false
    | (Neg Infinity, _) -> Some false
    | (Const c1, Const c2) when OurInt.Compare.(c1 > c2) -> Some true
    | (Abs b, Const z1) when OurInt.Compare.(OurInt.zero > z1) -> Some true
    | (b, Const z1) when OurInt.(equal z1 zero) -> (
      match b with
      | Max (b, _) when b > (Const OurInt.zero) |? false -> Some true
      | Max (_, b) when b > (Const OurInt.zero) |? false -> Some true
      | _ -> None
    )
    | (Const z1, b) when OurInt.(equal z1 zero) -> (
      match b with
      | Min (b, _) when (Const OurInt.zero) > b |? false -> Some true
      | Min (_, b) when (Const OurInt.zero) > b |? false -> Some true
      | _ -> None
    )
    | (b1, b2) -> None
  in
  Logger.with_log logger Logger.DEBUG
                  (fun () -> ">", ["condition", to_string b1 ^ ">" ^ to_string b2])
                  ~result:(Util.option_to_string Bool.to_string)
                  execute

let rec (>=) b1 b2 =
  let execute () =
    if equal b1 b2 then
      Some true
    else (
      match (b1, b2) with
      | (Infinity, _) -> Some true
      | (_, Neg Infinity) -> Some true
      | (Const c1, Const c2) when OurInt.Compare.(c1 >= c2) -> Some true
      | (Abs b, Const z1) when OurInt.Compare.(OurInt.zero >= z1) -> Some true
      | (b, Const z1) when OurInt.(equal z1 zero) -> (
        match b with
        | Max (b, _) when b >= (Const OurInt.zero) |? false -> Some true
        | Max (_, b) when b >= (Const OurInt.zero) |? false -> Some true
        | _ -> None
      )
      | (b, Const z1) when OurInt.(equal z1 zero) -> (
        match b with
        | Min (b, _) when (Const OurInt.zero) >= b |? false -> Some true
        | Min (_, b) when (Const OurInt.zero) >= b |? false -> Some true
        | _ -> None
      )
      | (b1, b2) -> None
    )
  in
  Logger.with_log logger Logger.DEBUG
                  (fun () -> ">=", ["condition", to_string b1 ^ ">=" ^ to_string b2])
                  ~result:(Util.option_to_string Bool.to_string)
                  execute

let (<) = flip (>)

let (<=) = flip (>=)

let (=~=) = equal

let rec simplify bound =
  let execute () =
    match bound with
      
    | Infinity -> Infinity

    | Var v -> Var v

    | Const c -> Const c

    | Abs b -> (
      match simplify b with
      | Abs b -> Abs b
      | Neg b -> Abs b
      | b when b >= (Const OurInt.zero) |? false -> b
      | b when b < (Const OurInt.zero) |? false -> simplify (Neg b)
      | b -> Abs b
    )

    (* Simplify terms with negation head *)
    | Neg b -> (
      match simplify b with
      | Const c -> Const (OurInt.neg c)
      | Max (b1, b2) -> simplify (Min (Neg b1, Neg b2))
      | Min (b1, b2) -> simplify (Max (Neg b1, Neg b2))
      | Sum (b1, b2) -> simplify (Sum (Neg b1, Neg b2))
      | Neg b -> b
      | Product (b1, b2) -> simplify (Product (Neg b1, b2))
      | b -> Neg b
    )

    (* Simplify terms with sum head *)
    | Sum (b1, b2) -> (
      match (simplify b1, simplify b2) with
      | (Const c, b) when OurInt.(c =~= zero) -> b
      | (b, Const c) when OurInt.(c =~= zero) -> b
      | (Const c1, Const c2) -> Const OurInt.(c1 + c2)
      | (Const c1, Sum (Const c2, b)) -> Sum (Const OurInt.(c1 + c2), b)
      | (Neg Infinity, Infinity) -> Const OurInt.zero
      | (Infinity, Neg Infinity) -> Const OurInt.zero
      | (_, Infinity) -> Infinity
      | (Infinity, _) -> Infinity
      | (_, Neg Infinity) -> Neg Infinity
      | (Neg Infinity, _) -> Neg Infinity
      | (Max (Const zero1, b1), Max (Const zero2, b2)) when OurInt.(zero1 =~= zero) && OurInt.(zero2 =~= zero) ->
         simplify (Max (Const OurInt.zero, Sum (b1, b2)))
      | (Const c1, Max (Const c2, b)) -> Max (Const OurInt.(c1 + c2), Sum (Const c1, b))
      | (b1, Neg b2) when equal b1 b2 -> Const OurInt.zero
      | (Neg b1, b2) when equal b1 b2 -> Const OurInt.zero
      | (b1, b2) when equal b1 b2 -> simplify (Product (Const (OurInt.of_int 2), b1))
      | (b1, Sum (b2, b3)) when Constructor.(b2 < b1) -> simplify (Sum (b2, Sum (b1, b3)))
      | (Sum (b1, b2), b3) when Constructor.(b3 < b2) -> simplify (Sum (Sum (b1, b3), b2))
      | (b1, b2) when Constructor.(b2 < b1) -> simplify (Sum (b2, b1))
      | (b1, b2) -> Sum (b1, b2)
    )

    (* Simplify terms with product head *)
    | Product (b1, b2) -> (
      match (simplify b1, simplify b2) with
      | (Const c, b) when OurInt.(c =~= one) -> b
      | (b, Const c) when OurInt.(c =~= one) -> b
      | (Const c, b) when OurInt.(c =~= zero) -> Const OurInt.zero
      | (b, Const c) when OurInt.(c =~= zero) -> Const OurInt.zero
      | (Const c, b) when OurInt.(c =~= neg one) -> simplify (Neg b)
      | (b, Const c) when OurInt.(c =~= neg one) -> simplify (Neg b)
      | (Infinity, b) when b >= Const OurInt.zero |? false -> Infinity
      | (b, Infinity) when b >= Const OurInt.zero |? false -> Infinity
      | (Infinity, b) when b <= Const OurInt.zero |? false -> Neg Infinity
      | (b, Infinity) when b <= Const OurInt.zero |? false -> Neg Infinity
      | (Neg Infinity, b) when b >= Const OurInt.zero |? false -> Neg Infinity
      | (b, Neg Infinity) when b >= Const OurInt.zero |? false -> Neg Infinity
      | (Neg Infinity, b) when b <= Const OurInt.zero |? false -> Infinity
      | (b, Neg Infinity) when b <= Const OurInt.zero |? false -> Infinity
      | (Abs b1, Abs b2) when equal b1 b2 -> Product (b1, b2)
      | (Max (Const zero1, b1), Max (Const zero2, b2)) when OurInt.(zero1 =~= zero) && OurInt.(zero2 =~= zero) ->
         simplify (Max (Const OurInt.zero, Product (b1, b2)))
      | (Max (Const zero1, b1), b2) when OurInt.(zero1 =~= zero) ->
         simplify (Max (Const OurInt.zero, Product (b1, b2)))
      | (b1, Product (b2, b3)) when Constructor.(b2 < b1) -> simplify (Product (b2, Product (b1, b3)))
      | (Product (b1, b2), b3) when Constructor.(b3 < b2) -> simplify (Product (Product (b1, b3), b2))
      | (b1, b2) when Constructor.(b2 < b1) -> simplify (Product (b2, b1))
      | (b1, b2) -> Product (b1, b2)
    )
                        
    (* Simplify terms with pow head *)
    | Pow (value, exponent) -> (
       match simplify exponent with
       | Infinity when OurInt.Compare.(value > OurInt.zero) -> Infinity
       | Neg Infinity when OurInt.Compare.(value > OurInt.zero) -> Const OurInt.zero
       | Const c -> Const OurInt.(pow value (to_int c))
       (* TODO Do not use OurInt.to_int *)
       | Max (Const c, b) -> Max (Const (OurInt.pow value (OurInt.to_int c)), Pow (value, b))
       | _ when OurInt.(equal value zero) -> Const OurInt.zero
       | _ when OurInt.(equal value one) -> Const OurInt.one
       | exponent -> Pow (value, exponent)
    )

    (* Simplify terms with min head *)
    | Min (b1, b2) ->
       let (b1, b2) = (simplify b1, simplify b2) in
       if b1 >= b2 |? false then
         b2
       else if b2 >= b1 |? false then
         b1
       else (
         match (b1, b2) with
         | (b1, Min (b2, b3)) when Constructor.(b2 < b1) -> simplify (Min (b2, Min (b1, b3)))
         | (Min (b1, b2), b3) when Constructor.(b3 < b2) -> simplify (Min (Min (b1, b3), b2))
         | (b1, b2) when Constructor.(b2 < b1) -> simplify (Min (b2, b1))
         | (b1, b2) -> Min (b1, b2)
       )

    (* Simplify terms with max head *)
    | Max (b1, b2) ->
       let (b1, b2) = (simplify b1, simplify b2) in
       if b1 >= b2 |? false then
         b1
       else if b2 >= b1 |? false then
         b2
       else (
         match (b1, b2) with
         | (b1, Max (b2, b3)) when Constructor.(b2 < b1) -> simplify (Max (b2, Max (b1, b3)))
         | (Max (b1, b2), b3) when Constructor.(b3 < b2) -> simplify (Max (Max (b1, b3), b2))
         | (b1, b2) when Constructor.(b2 < b1) -> simplify (Max (b2, b1))
         | (b1, b2) -> Max (b1, b2)
       )
  in
  Logger.with_log logger Logger.DEBUG
                  (fun () -> "simplify", ["input", to_string bound])
                  ~result:to_string
                  execute

type outer_t = t
module BaseMathImpl : (PolyTypes.BaseMath with type t = outer_t) =
  struct
    type t = outer_t
           
    let zero = Const (OurInt.zero)
             
    let one = Const (OurInt.one)
            
    let neg bound = simplify (Neg bound)
              
    let add b1 b2 =
      simplify (Sum (b1, b2))
      
    let mul b1 b2 =
      simplify (Product (b1, b2))
      
    let pow bound n =
      bound
      |> Enum.repeat ~times:n
      |> Enum.fold mul one
      |> simplify
      
  end
include PolyTypes.MakeMath(BaseMathImpl)

let sum bounds =
  try
    bounds |> Enum.reduce add |> simplify
  with Not_found -> zero
  
let product bounds =
  try
    bounds |> Enum.reduce mul |> simplify
  with Not_found -> one
  
let sub t1 t2 =
  simplify (add t1 (neg t2))
      
let of_poly =
  Polynomial.fold ~const:of_constant ~var:of_var ~neg:neg ~plus:add ~times:mul ~pow:pow
              
let of_int i = Const (OurInt.of_int i)
                  
let to_int poly = raise (Failure "TODO: Not possible")
                
let of_var_string str = Var (Var.of_string str)

let infinity = Infinity

let minus_infinity = Neg Infinity

let is_infinity = equal Infinity
                   
let rec fold ~const ~var ~neg ~plus ~times ~pow ~exp ~min ~max ~abs ~inf p =
  let fold_ = fold ~const ~var ~neg ~plus ~times ~pow ~exp ~min ~max ~abs ~inf in
  match p with
  | Infinity -> inf
  | Var v -> var v
  | Abs b -> abs (fold_ b)
  | Const c -> const c
  | Max (b1, b2) -> max (fold_ b1) (fold_ b2)
  | Min (b1, b2) -> min (fold_ b1) (fold_ b2)
  | Neg b -> neg (fold_ b)
  | Pow (value, n) -> exp value (fold_ n)
  | Sum (b1, b2) -> plus (fold_ b1) (fold_ b2)
  | Product (b1, b2) -> times (fold_ b1) (fold_ b2)
                    
let max b1 b2 =
  simplify (Max (b1, b2))

let min b1 b2 =
  simplify (Min (b1, b2))

let maximum bounds =
  try
    bounds |> Enum.reduce max |> simplify
  with Not_found -> minus_infinity

let minimum bounds =
  try
    bounds |> Enum.reduce min |> simplify
  with Not_found -> infinity

let exp value b =
  simplify (Pow (value, b))

let abs bound =
  simplify (Abs bound)

let is_var = function
  | Var _ -> true
  | _ -> false

let substitute_f substitution =
  fold ~const:of_constant ~var:substitution ~neg:neg ~plus:add ~times:mul ~pow:pow ~exp:exp ~min:min ~max:max ~abs:abs ~inf:infinity
  
let substitute var ~replacement =
  substitute_f (fun target_var ->
      if Var.(var =~= target_var) then replacement else of_var target_var
    )

let substitute_all substitution =
  let module VarMap = Map.Make(Var) in
  substitute_f (fun var ->
      VarMap.find_default (of_var var) var substitution
    )                      

let rec vars = function
  | Infinity -> VarSet.empty
  | Var v -> VarSet.singleton v
  | Abs b -> vars b
  | Const _ -> VarSet.empty
  | Max (b1, b2) -> VarSet.union (vars b1) (vars b2)
  | Min (b1, b2) -> VarSet.union (vars b1) (vars b2)
  | Neg b -> vars b
  | Pow (v,b) -> vars b
  | Sum (b1, b2) -> VarSet.union (vars b1) (vars b2)
  | Product (b1, b2) -> VarSet.union (vars b1) (vars b2)

let degree n = raise (Failure "degree for MinMaxPolynomial not yet implemented")
let rename map p = raise (Failure "rename for MinMaxPolynomial not yet implemented")
let eval p valuation = raise (Failure "eval for MinMaxPolynomial not yet implemented")
let eval_f p valuation = raise (Failure "eval_f for MinMaxPolynomial not yet implemented")
let of_string p = raise (Failure "of_string for MinMaxPolynomial not yet implemented")
