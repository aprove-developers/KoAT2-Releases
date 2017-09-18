open Batteries
   
module Make(P : PolyTypes.Polynomial) =
  struct
    module Polynomial_ = P
    module Value = P.Value
    module Var = P.Var
    module Valuation_ = P.Valuation_
    module RenameMap_ = P.RenameMap_
                          
    (* Minus Infinity is max of an empty list *)
    (* Infinity is min of an empty list *)
    type t =
      | Poly of Polynomial_.t
      | Max of t list
      | Min of t list
      | Neg of t
      | Pow of Value.t * t
      | Sum of t list
      | Product of t list [@@deriving eq]

    let of_poly p = Poly p
              
    let of_constant c = of_poly (Polynomial_.from_constant c)

    let of_var v = of_poly (Polynomial_.from_var v)

    let infinity = Min []

    let minus_infinity = Max []

    let rec fold ~const ~var ~neg ~plus ~times ~pow ~exp ~min ~max ~inf p =
      let fold_ = fold ~const ~var ~neg ~plus ~times ~pow ~exp ~min ~max ~inf in
      match p with
      | Poly p -> Polynomial_.fold ~const ~var ~neg ~plus ~times ~pow p
      | Max bounds -> List.fold_left (fun b bound -> max b (fold_ bound)) (neg inf) bounds
      | Min bounds -> List.fold_left (fun b bound -> min b (fold_ bound)) inf bounds
      | Neg b -> neg (fold_ b)
      | Pow (value, n) -> exp value (fold_ n)
      | Sum bounds -> List.fold_left (fun b bound -> plus b (fold_ bound)) (const Value.zero) bounds
      | Product bounds -> List.fold_left (fun b bound -> times b (fold_ bound)) (const Value.one) bounds                    
                                     
    let rec simplify = function
      | Poly p -> Poly p

      (* Simplify terms with negation head *)
      | Neg (Poly p) -> Poly (Polynomial_.neg p)
      | Neg (Max bounds) -> simplify (Min (List.map (fun b -> Neg b) bounds))
      | Neg (Min bounds) -> simplify (Max (List.map (fun b -> Neg b) bounds))
      | Neg (Sum bounds) -> simplify (Sum (List.map (fun b -> Neg b) bounds))
      | Neg (Neg b) -> simplify b
      | Neg (Product (b::bs)) -> simplify (Product (Neg b :: bs))
      | Neg b -> Neg (simplify b)

      (* Simplify terms with sum head *)
      | Sum [] -> of_constant Value.zero
      | Sum [b] -> simplify b
      | Sum (Sum bounds :: bs) -> simplify (Sum (List.append bounds bs))
      | Sum (Poly p :: bs) when Polynomial_.(p =~= zero) -> simplify (Sum bs)
      | Sum [Poly p1; Poly p2] -> Poly Polynomial_.(p1 + p2)
      | Sum (Poly p1 :: Poly p2 :: bs) -> simplify (Sum (Poly Polynomial_.(p1 + p2) :: bs))
      | Sum (Max bounds1 :: Max bounds2 :: bs) -> simplify (Sum (Max (List.map (fun (b1, b2) -> Sum [b1; b2]) (List.cartesian_product bounds1 bounds2)) :: bs))
      | Sum (Min bounds1 :: Min bounds2 :: bs) -> simplify (Sum (Min (List.map (fun (b1, b2) -> Sum [b1; b2]) (List.cartesian_product bounds1 bounds2)) :: bs))
      | Sum (Poly p :: Max bounds :: bs) -> simplify (Sum (Max bounds :: Poly p :: bs))
      | Sum (Max bounds :: Poly p :: bs) -> simplify (Sum ((Max (List.map (fun b -> Sum [Poly p; b]) bounds)) :: bs))
      | Sum (Poly p :: Min bounds :: bs) -> simplify (Sum (Min bounds :: Poly p :: bs))
      | Sum (Min bounds :: Poly p :: bs) -> simplify (Sum ((Min (List.map (fun b -> Sum [Poly p; b]) bounds)) :: bs))
      | Sum bounds -> Sum (List.map simplify bounds)

      (* Simplify terms with product head *)
      | Product [] -> of_constant Value.one
      | Product [b] -> simplify b
      | Product (Product bounds :: bs) -> simplify (Product (List.append bounds bs))
      | Product (Poly p :: bs) when Polynomial_.(p =~= one) -> simplify (Product bs)
      | Product bounds -> Product (List.map simplify bounds)

      (* Simplify terms with pow head *)
      | Pow (value, bound) ->
         if value == Value.zero then of_constant Value.zero
         else if value == Value.one then of_constant Value.one
         else if bound == of_constant Value.zero then of_constant Value.one
         else if bound == of_constant Value.one then simplify bound
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
               
        let zero = of_poly Polynomial_.zero
                 
        let one = of_poly Polynomial_.one
            
        let neg b = simplify (Neg b)
               
        let add b1 b2 =
          simplify (Sum [b1; b2])
                      
        let mul b1 b2 =
          simplify (Product [b1; b2])
                      
        let pow b n =
          List.fold_left mul one (List.of_enum (Enum.repeat ~times:n b))
            
      end
    include PolyTypes.MakeMath(BaseMathImpl)
          
    module BasePartialOrderImpl : (PolyTypes.BasePartialOrder with type t = outer_t) =
      struct
        type t = outer_t
               
        let (=~=) = equal
                       
        let rec (>) b1 b2 = match (b1, b2) with
          | (Poly p1, Poly p2) -> Polynomial_.(>) p1 p2
          | (_, Max []) -> Some false
          | (Max [], _) -> Some true
          | (Max bounds1, Max bounds2) -> Some (List.exists (fun b1 -> List.for_all (fun b2 -> Option.default false (b1 > b2)) bounds2) bounds1)
          | (Max bounds, Poly p) -> Some (List.exists (fun b -> Option.default false (b > Poly p)) bounds)
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
                       
    let substitute_f substitution =
      fold ~const:of_constant ~var:substitution ~neg:neg ~plus:add ~times:mul ~pow:pow ~exp:exp ~min:min ~max:max ~inf:infinity
          
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
      | Poly p -> Polynomial_.to_string p
      | Max [] -> "inf"
      | Min [] -> "neg inf"
      | Max bounds -> "max {" ^ String.concat ", " (List.map to_string bounds) ^ "}"
      | Min bounds -> "min {" ^ String.concat ", " (List.map to_string bounds) ^ "}"
      | Neg b -> "neg " ^ to_string b
      | Pow (v,b) -> "(" ^ Value.to_string v ^ "**" ^ to_string b ^ ")"
      | Sum bounds -> "sum {" ^ String.concat ", " (List.map to_string bounds) ^ "}"
      | Product bounds -> "product {" ^ String.concat ", " (List.map to_string bounds) ^ ")}"

    let rec vars = function
      | Poly p -> Polynomial_.vars p
      | Max bounds -> List.fold_left Set.union Set.empty (List.map vars bounds)
      | Min bounds -> List.fold_left Set.union Set.empty (List.map vars bounds)
      | Neg b -> vars b
      | Pow (v,b) -> vars b
      | Sum bounds -> List.fold_left Set.union Set.empty (List.map vars bounds)
      | Product bounds -> List.fold_left Set.union Set.empty (List.map vars bounds)

    let degree n = raise (Failure "degree for MinMaxPolynomial not yet implemented")
    let rename map p = raise (Failure "rename for MinMaxPolynomial not yet implemented")
    let to_z3 ctx p = raise (Failure "to_z3 for MinMaxPolynomial not yet implemented")
    let eval p valuation = raise (Failure "eval for MinMaxPolynomial not yet implemented")
    let eval_f p valuation = raise (Failure "eval_f for MinMaxPolynomial not yet implemented")
    let of_string p = raise (Failure "of_string for MinMaxPolynomial not yet implemented")

  end
