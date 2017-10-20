open Batteries
open Polynomials
   
module Valuation_ = Valuation.Make(OurInt)
type valuation = Valuation.Make(OurInt).t
                  
type polynomial = Polynomial.t
type value = OurInt.t
                
(* Minus Infinity is max of an empty list *)
(* Infinity is min of an empty list *)
type t =
  | Var of Var.t
  | Abs of Var.t
  | Const of OurInt.t
  | Max of t list
  | Min of t list
  | Neg of t
  | Pow of OurInt.t * t
  | Sum of t * t
  | Product of t * t [@@deriving eq]

let of_var v = Var v

let of_constant c = Const c

let rec simplify = function
  | Var v -> Var v

  | Abs v -> Abs v

  | Const c -> Const c

  (* Simplify terms with negation head *)
  | Neg (Const c) -> Const (OurInt.neg c)
  | Neg (Max bounds) -> simplify (Min (List.map (fun b -> Neg b) bounds))
  | Neg (Min bounds) -> simplify (Max (List.map (fun b -> Neg b) bounds))
  | Neg (Sum (b1, b2)) -> simplify (Sum (Neg b1, Neg b2))
  | Neg (Neg b) -> simplify b
  | Neg (Product (b1, b2)) -> simplify (Product (Neg b1, b2))
  | Neg b -> Neg (simplify b)

  (* Simplify terms with sum head *)
  | Sum (Const c, b) when OurInt.(c =~= zero) -> simplify b
  | Sum (b, Const c) when OurInt.(c =~= zero) -> simplify b
  | Sum (Const c1, Const c2) -> Const OurInt.(c1 + c2)
  | Sum (b1, b2) -> Sum (simplify b1, simplify b2)

  (* Simplify terms with product head *)
  | Product (Const c, b) when OurInt.(c =~= one) -> simplify b
  | Product (b, Const c) when OurInt.(c =~= one) -> simplify b
  | Product (b1, b2) -> Product (simplify b1, simplify b2)

  (* Simplify terms with pow head *)
  | Pow (value, bound) ->
     if value == OurInt.zero then of_constant OurInt.zero
     else if value == OurInt.one then of_constant OurInt.one
     else if bound == of_constant OurInt.zero then of_constant OurInt.one
     else if bound == of_constant OurInt.one then simplify bound
     else Pow (value, simplify bound)

  (* Simplify terms with min head *)
  | Min [] -> Min []
  | Min [b] -> simplify b
  | Min (Min bounds :: bs) -> simplify (Min (List.append bounds bs))
  | Min bounds -> Min (List.map simplify bounds)

  (* Simplify terms with max head *)
  | Max [] -> Max []
  | Max [b] -> simplify b
  | Max (Max bounds :: bs) -> simplify (Max (List.append bounds bs))
  | Max bounds -> Max (List.map simplify bounds)
                
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
      
let of_poly =
  Polynomial.fold ~const:of_constant ~var:of_var ~neg:neg ~plus:add ~times:mul ~pow:pow
              
let of_int i = Const (OurInt.of_int i)
                  
let of_var_string str = Var (Var.of_string str)

let infinity = Min []

let minus_infinity = Max []

let rec fold ~const ~var ~neg ~plus ~times ~pow ~exp ~min ~max ~abs ~inf p =
  let fold_ = fold ~const ~var ~neg ~plus ~times ~pow ~exp ~min ~max ~abs ~inf in
  match p with
  | Var v -> var v
  | Abs v -> abs v
  | Const c -> const c
  | Max bounds -> List.fold_left (fun b bound -> max b (fold_ bound)) (neg inf) bounds
  | Min bounds -> List.fold_left (fun b bound -> min b (fold_ bound)) inf bounds
  | Neg b -> neg (fold_ b)
  | Pow (value, n) -> exp value (fold_ n)
  | Sum (b1, b2) -> plus (fold_ b1) (fold_ b2)
  | Product (b1, b2) -> times (fold_ b1) (fold_ b2)
                    
module BasePartialOrderImpl : (PolyTypes.BasePartialOrder with type t = outer_t) =
  struct
    type t = outer_t
           
    let (=~=) = equal
              
    let rec (>) b1 b2 = match (b1, b2) with
      | (_, Max []) -> Some false
      | (Max [], _) -> Some true
      | (Max bounds1, Max bounds2) -> Some (List.exists (fun b1 -> List.for_all (fun b2 -> Option.default false (b1 > b2)) bounds2) bounds1)
      (* TODO Add more cases *)
      | (b1, b2) -> None
                  
  end
include PolyTypes.MakePartialOrder(BasePartialOrderImpl)

let max b1 b2 = 
  simplify (Max [b1; b2])

let min b1 b2 =
  simplify (Min [b1; b2])

let maximum bounds =
  simplify (Max bounds)
  
let minimum bounds =
  simplify (Min bounds)

let exp value b =
  simplify (Pow (value, b))

let abs var =
  Abs var

let is_var = function
  | Var _ -> true
  | Abs _ -> true
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

let rec to_string = function
  | Var v -> Var.to_string v
  | Abs v -> "|" ^ Var.to_string v ^ "|"
  | Const c -> OurInt.to_string c
  | Max [] -> "inf"
  | Min [] -> "neg inf"
  | Max bounds -> "max {" ^ String.concat ", " (List.map to_string bounds) ^ "}"
  | Min bounds -> "min {" ^ String.concat ", " (List.map to_string bounds) ^ "}"
  | Neg b -> "neg " ^ to_string b
  | Pow (v,b) -> "(" ^ OurInt.to_string v ^ "**" ^ to_string b ^ ")"
  | Sum (b1, Sum (b2, b3)) -> "(" ^ to_string b1 ^ "+" ^ to_string b2 ^ "+" ^ to_string b3 ^ ")"
  | Sum (Sum (b1, b2), b3) -> "(" ^ to_string b1 ^ "+" ^ to_string b2 ^ "+" ^ to_string b3 ^ ")"
  | Sum (b1, b2) -> "(" ^ to_string b1 ^ "+" ^ to_string b2 ^ ")"
  | Product (b1, Product (b2, b3)) -> "(" ^ to_string b1 ^ "*" ^ to_string b2 ^ "*" ^ to_string b3 ^ ")"
  | Product (Product (b1, b2), b3) -> "(" ^ to_string b1 ^ "*" ^ to_string b2 ^ "*" ^ to_string b3 ^ ")"
  | Product (b1, b2) -> "(" ^ to_string b1 ^ "*" ^ to_string b2 ^ ")"

let rec vars = function
  | Var v -> VarSet.singleton v
  | Abs v -> VarSet.singleton v
  | Const _ -> VarSet.empty
  | Max bounds -> List.fold_left VarSet.union VarSet.empty (List.map vars bounds)
  | Min bounds -> List.fold_left VarSet.union VarSet.empty (List.map vars bounds)
  | Neg b -> vars b
  | Pow (v,b) -> vars b
  | Sum (b1, b2) -> VarSet.union (vars b1) (vars b2)
  | Product (b1, b2) -> VarSet.union (vars b1) (vars b2)

let degree n = raise (Failure "degree for MinMaxPolynomial not yet implemented")
let rename map p = raise (Failure "rename for MinMaxPolynomial not yet implemented")
let eval p valuation = raise (Failure "eval for MinMaxPolynomial not yet implemented")
let eval_f p valuation = raise (Failure "eval_f for MinMaxPolynomial not yet implemented")
let of_string p = raise (Failure "of_string for MinMaxPolynomial not yet implemented")
