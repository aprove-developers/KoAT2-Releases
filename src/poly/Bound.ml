open Batteries
open Polynomials
   
module Valuation_ = Valuation.Make(OurInt)
type valuation = Valuation.Make(OurInt).t
                  
type polynomial = Polynomial.t
type value = OurInt.t
                
(* Minus Infinity is max of an empty list *)
(* Infinity is min of an empty list *)
type t =
  | Infinity
  | Var of Var.t
  | Abs of Var.t
  | Const of OurInt.t
  | Max of t * t
  | Min of t * t
  | Neg of t
  | Pow of OurInt.t * t
  | Sum of t * t
  | Product of t * t [@@deriving eq, ord]

let of_var v = Var v

let of_abs_var v = Abs v

let of_constant c = Const c

let rec simplify = function
  | Infinity -> Infinity

  | Var v -> Var v

  | Abs v -> Abs v

  | Const c -> Const c

  (* Simplify terms with negation head *)
  | Neg (Const c) -> Const (OurInt.neg c)
  | Neg (Max (b1, b2)) -> simplify (Min (Neg b1, Neg b2))
  | Neg (Min (b1, b2)) -> simplify (Max (Neg b1, Neg b2))
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
  | Min (b1, b2) ->
     (* Optimization if we have structural equality *)
     if equal b1 b2 then
       b1
     else
       Min (simplify b1, simplify b2)

  (* Simplify terms with max head *)
  | Max (b1, b2) ->
     (* Optimization if we have structural equality *)
     if equal b1 b2 then
       b1
     else
       Max (simplify b1, simplify b2)
                
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
                  
let to_int poly = raise (Failure "TODO: Not possible")
                
let of_var_string str = Var (Var.of_string str)

let infinity = Infinity

let minus_infinity = Neg Infinity

let rec fold ~const ~var ~neg ~plus ~times ~pow ~exp ~min ~max ~abs ~inf p =
  let fold_ = fold ~const ~var ~neg ~plus ~times ~pow ~exp ~min ~max ~abs ~inf in
  match p with
  | Infinity -> inf
  | Var v -> var v
  | Abs v -> abs v
  | Const c -> const c
  | Max (b1, b2) -> max (fold_ b1) (fold_ b2)
  | Min (b1, b2) -> min (fold_ b1) (fold_ b2)
  | Neg b -> neg (fold_ b)
  | Pow (value, n) -> exp value (fold_ n)
  | Sum (b1, b2) -> plus (fold_ b1) (fold_ b2)
  | Product (b1, b2) -> times (fold_ b1) (fold_ b2)
                    
module BasePartialOrderImpl : (PolyTypes.BasePartialOrder with type t = outer_t) =
  struct
    type t = outer_t
           
    let (=~=) = equal
              
    let rec (>) b1 b2 =
      match (b1, b2) with
      | (_, Infinity) -> Some false
      | (Infinity, _) -> Some true
      (* TODO Add more cases *)
      | (b1, b2) -> None
                  
  end
include PolyTypes.MakePartialOrder(BasePartialOrderImpl)

let max b1 b2 =
  simplify (Max (b1, b2))

let min b1 b2 =
  simplify (Min (b1, b2))

let maximum =
  List.fold_left max (minus_infinity)
  
let minimum =
  List.fold_left min infinity

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
  | Infinity -> "inf"
  | Max (b1, b2) -> "max {" ^ to_string b1 ^ ", " ^ to_string b1 ^ "}"
  | Min (b1, b2) -> "min {" ^ to_string b1 ^ ", " ^ to_string b1 ^ "}"
  | Neg b -> "neg " ^ to_string b
  | Pow (v,b) -> "(" ^ OurInt.to_string v ^ "**" ^ to_string b ^ ")"
  | Sum (b1, Sum (b2, b3)) -> "(" ^ to_string b1 ^ "+" ^ to_string b2 ^ "+" ^ to_string b3 ^ ")"
  | Sum (Sum (b1, b2), b3) -> "(" ^ to_string b1 ^ "+" ^ to_string b2 ^ "+" ^ to_string b3 ^ ")"
  | Sum (b1, b2) -> "(" ^ to_string b1 ^ "+" ^ to_string b2 ^ ")"
  | Product (b1, Product (b2, b3)) -> "(" ^ to_string b1 ^ "*" ^ to_string b2 ^ "*" ^ to_string b3 ^ ")"
  | Product (Product (b1, b2), b3) -> "(" ^ to_string b1 ^ "*" ^ to_string b2 ^ "*" ^ to_string b3 ^ ")"
  | Product (b1, b2) -> "(" ^ to_string b1 ^ "*" ^ to_string b2 ^ ")"

let rec vars = function
  | Infinity -> VarSet.empty
  | Var v -> VarSet.singleton v
  | Abs v -> VarSet.singleton v
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
