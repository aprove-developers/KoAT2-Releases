open Batteries
open PolyTypes
   
module MakeMinMaxPolynomial(Var : ID)(Value : Number.Numeric) =
  struct
    module Valuation = Valuation.MakeValuation(Var)(Value)
    module RenameMap = Map.Make(Var)
    module PolynomialAST = PolynomialAST(Var)
    module Polynomial = Polynomials.MakePolynomial(Var)(Value)
                          
    type value = Value.t
    type valuation = Valuation.t
    type var = Var.t
    type rename_map = var RenameMap.t
    type polynomial_ast = PolynomialAST.t

    (* Infinity is max of an empty list *)
    (* Minus Infinity is min of an empty list *)
    type t =
      | Poly of Polynomial.t
      | Max of t list
      | Min of t list
      | Neg of t
      | Pow of value * t
      | Sum of t list
      | Product of t list
              
    let of_poly p = Poly p
              
    let of_constant c = of_poly (Polynomial.from_constant c)

    let zero = of_poly Polynomial.zero
             
    let one = of_poly Polynomial.one
            
    let rec simplify = function
      | Neg (Neg b) -> simplify b
      | Pow (v,b) ->
         if v == Value.zero then zero
         else if v == Value.one then one
         else if b == zero then one
         else if b == one then of_constant v
         else Pow (v,b)
      | b -> b

    let rec neg = function
      | Poly p -> Poly (Polynomial.neg p)
      | Max bounds -> Min (List.map neg bounds)
      | Min bounds -> Max (List.map neg bounds)
      | Neg b -> b
      | Sum bounds -> Sum (List.map neg bounds)
      | Product (b::bs) -> Product (neg b :: bs)
      (* TODO Add more simplification? *)
      | b -> Neg b
              
    let rec add b1 b2 = match (b1, b2) with
      | (Poly p1, Poly p2) -> Poly (Polynomial.add p1 p2)
      | (Max bounds1, Max bounds2) -> Max (List.map (uncurry add) (List.cartesian_product bounds1 bounds2))
      | (Min bounds1, Min bounds2) -> Min (List.map (uncurry add) (List.cartesian_product bounds1 bounds2))
      | (Max bounds, Poly p) -> add (Max bounds) (Max [Poly p])
      | (Min bounds, Poly p) -> add (Min bounds) (Min [Poly p])
      (* TODO Add more simplification *)
      | (b1, b2) -> Sum [b1; b2]
                  
    let sum = List.fold_left add zero
            
    let sub b1 b2 = add b1 (neg b2)
                  
    let mul b1 b2 = match (b1, b2) with
      (* TODO Add more simplification *)
      | (b1, b2) -> Product [b1; b2]

    let product = List.fold_left mul one
              
    let pow b n = match (b, n) with
      (* TODO Add more simplification *)
      | (b, n) ->
         if b == zero then zero
         else if b == one then one
         else if n == 0 then one
         else if n == 1 then b
         else product (List.of_enum (Enum.repeat ~times:n b))

    type outer_t = t
    module BasePartialOrderImpl : (BasePartialOrder with type t = outer_t) =
      struct
        type t = outer_t
               
        let (==) b1 b2 = b1 == b2
                       
        let rec (>) b1 b2 = match (b1, b2) with
          | (Poly p1, Poly p2) -> Polynomial.(>) p1 p2
          | (_, Max []) -> Some false
          | (Max [], _) -> Some true
          | (Max bounds1, Max bounds2) -> Some (List.exists (fun b1 -> List.for_all (fun b2 -> Option.default false (b1 > b2)) bounds2) bounds1)
          | (Max bounds, Poly p) -> Some (List.exists (fun b -> Option.default false (b > Poly p)) bounds)
          (* TODO Add more cases *)
          | (b1, b2) -> None
                      
      end
    include MakePartialOrder(BasePartialOrderImpl)
                  
    let max b1 b2 = match (b1, b2) with
      (* TODO Add more simplification *)
      | (b1, b2) -> Max [b1; b2]

    let min b1 b2 = match (b1, b2) with
      (* TODO Add more simplification *)
      | (b1, b2) -> Min [b1; b2]

    let rec to_string = function
      | Poly p -> Polynomial.to_string p
      | Max bounds -> String.concat "" ["max{"; String.concat ", " (List.map to_string bounds); "}"]
      | Min bounds -> String.concat "" ["min{"; String.concat ", " (List.map to_string bounds); "}"]
      | Neg b -> String.concat "" ["-("; to_string b; ")"]
      | Pow (v,b) -> String.concat "" ["("; Value.to_string v; "^"; to_string b; ")"]
      | Sum bounds -> String.concat "" ["("; String.concat "+" (List.map to_string bounds); ")"]
      | Product bounds -> String.concat "" ["("; String.concat "*" (List.map to_string bounds); ")"]

    let rec vars = function
      | Poly p -> Polynomial.vars p
      | Max bounds -> List.flatten (List.map vars bounds)
      | Min bounds -> List.flatten (List.map vars bounds)
      | Neg b -> vars b
      | Pow (v,b) -> vars b
      | Sum bounds -> List.flatten (List.map vars bounds)
      | Product bounds -> List.flatten (List.map vars bounds)

    let degree = raise (Failure "Not yet implemented")
    let rename = raise (Failure "Not yet implemented")
    let to_z3 = raise (Failure "Not yet implemented")
    let eval = raise (Failure "Not yet implemented")
    let of_string = raise (Failure "Not yet implemented")

  end
