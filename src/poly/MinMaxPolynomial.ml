open Batteries
   
module Make(Var : PolyTypes.ID)(Value : PolyTypes.Field) =
  struct
    module Valuation_ = Valuation.Make(Var)(Value)
    module RenameMap_ = RenameMap.Make(Var)
    module Polynomial_ = Polynomials.Make(Var)(Value)
                          
    (* Minus Infinity is max of an empty list *)
    (* Infinity is min of an empty list *)
    type t =
      | Poly of Polynomial_.t
      | Max of t list
      | Min of t list
      | Neg of t
      | Pow of Value.t * t
      | Sum of t list
      | Product of t list
              
    module Var = Var
    module Value = Value

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
                                     
    let of_poly p = Poly p
              
    let of_constant c = of_poly (Polynomial_.from_constant c)

    let of_var v = of_poly (Polynomial_.from_var v)

    type outer_t = t
    module BaseMathImpl : (PolyTypes.BaseMath with type t = outer_t) =
      struct
        type t = outer_t
               
        let zero = of_poly Polynomial_.zero
                 
        let one = of_poly Polynomial_.one
            
        let rec neg = function
          | Poly p -> Poly (Polynomial_.neg p)
          | Max bounds -> Min (List.map neg bounds)
          | Min bounds -> Max (List.map neg bounds)
          | Neg b -> b
          | Sum bounds -> Sum (List.map neg bounds)
          | Product (b::bs) -> Product (neg b :: bs)
          (* TODO Add more simplification? *)
          | b -> Neg b
               
        let rec add b1 b2 = match (b1, b2) with
          | (Poly p1, Poly p2) -> Poly (Polynomial_.add p1 p2)
          | (Max bounds1, Max bounds2) -> Max (List.map (uncurry add) (List.cartesian_product bounds1 bounds2))
          | (Min bounds1, Min bounds2) -> Min (List.map (uncurry add) (List.cartesian_product bounds1 bounds2))
          | (Max bounds, Poly p) -> add (Max bounds) (Max [Poly p])
          | (Min bounds, Poly p) -> add (Min bounds) (Min [Poly p])
          (* TODO Add more simplification *)
          | (b1, b2) -> Sum [b1; b2]
                      
        let mul b1 b2 = match (b1, b2) with
          (* TODO Add more simplification *)
          | (b1, b2) -> Product [b1; b2]
                      
        let pow b n = match (b, n) with
          (* TODO Add more simplification *)
          | (b, n) ->
             if b == zero then zero
             else if b == one then one
             else if n == 0 then one
             else if n == 1 then b
             else List.fold_left mul one (List.of_enum (Enum.repeat ~times:n b))
            
      end
    include PolyTypes.MakeMath(BaseMathImpl)
          
    module BasePartialOrderImpl : (PolyTypes.BasePartialOrder with type t = outer_t) =
      struct
        type t = outer_t
               
        let (=~=) b1 b2 = b1 == b2
                       
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
                  
    let max b1 b2 = match (b1, b2) with
      (* TODO Add more simplification *)
      | (b1, b2) -> Max [b1; b2]

    let min b1 b2 = match (b1, b2) with
      (* TODO Add more simplification *)
      | (b1, b2) -> Min [b1; b2]

    let maximum bounds = Max bounds
                  
    let minimum bounds = Min bounds

    let infinity = minimum []

    let minus_infinity = maximum []

    let exp value b = Pow (value, b)
                       
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
      | Max bounds -> "max{" ^ String.concat ", " (List.map to_string bounds) ^ "}"
      | Min bounds -> "min{" ^ String.concat ", " (List.map to_string bounds) ^ "}"
      | Neg b -> "-(" ^ to_string b ^ ")"
      | Pow (v,b) -> "(" ^ Value.to_string v ^ "^" ^ to_string b ^ ")"
      | Sum bounds -> "(" ^ String.concat "+" (List.map to_string bounds) ^ ")"
      | Product bounds -> "(" ^ String.concat "*" (List.map to_string bounds) ^ ")"

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
    let of_string p = raise (Failure "of_string for MinMaxPolynomial not yet implemented")

  end
