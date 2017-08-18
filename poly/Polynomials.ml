open Batteries
   
module Make(Var : PolyTypes.ID)(Value : Number.Numeric) =
  struct
    module Monomial_ = Monomials.Make(Var)(Value)
    module ScaledMonomial_ = ScaledMonomials.Make(Var)(Value)
    module Valuation_ = Valuation.Make(Var)(Value)
    module RenameMap_ = RenameMap.Make(Var)
                      
    type t = ScaledMonomial_.t list 
    type monomial = Monomial_.t
    type scaled_monomial = ScaledMonomial_.t
                         
    module Var = Var
    module Value = Value

    let make = List.map (fun (coeff, mon) -> ScaledMonomial_.make coeff mon)

    let lift coeff mon = [ScaledMonomial_.make coeff mon]

    let fold ~const ~var ~neg ~plus ~times ~pow =
      List.fold_left (fun b scaled -> plus b (ScaledMonomial_.fold ~const ~var ~times ~pow scaled)) (const Value.zero)
                    
    let degree poly =
      List.max (List.map (ScaledMonomial_.degree) poly )
      
    let coeff mon poly =
         poly
      |> List.filter (fun scaled -> Monomial_.(==) (ScaledMonomial_.monomial scaled) mon)
      |> List.map ScaledMonomial_.coeff
      |> List.fold_left Value.add Value.zero

    let delete_monomial mon poly =
      List.filter (fun x -> not (Monomial_.(==) (ScaledMonomial_.monomial x) mon)) poly

    let rec simplify_partial_simplified poly =
      match poly with 
      | [] -> []
      | scaled::tail ->
         let curr_monom = ScaledMonomial_.monomial scaled in
         let curr_coeff = coeff curr_monom poly in
         if (Value.equal curr_coeff Value.zero) then (simplify_partial_simplified (delete_monomial curr_monom tail))
         else (ScaledMonomial_.make curr_coeff curr_monom) :: (simplify_partial_simplified (delete_monomial curr_monom tail) )

    let simplify poly =
      simplify_partial_simplified (List.map (ScaledMonomial_.simplify) poly)

    let to_string_simplified poly = 
      if (poly == []) then "0" 
      else 
        String.concat "+" (List.map ScaledMonomial_.to_string poly)

    let to_string poly = to_string_simplified (simplify poly)

    let of_string poly = raise (Failure "of_string for Polynomial not yet implemented") (* TODO Use ocamlyacc *)

    let rec equal_simplified poly1 poly2 =
      List.length poly1 == List.length poly2 &&
        match poly1 with
        | [] -> true
        | scaled :: tail ->
           let curr_mon = ScaledMonomial_.monomial scaled in
           let curr_coeff = ScaledMonomial_.coeff scaled in
           Value.equal curr_coeff (coeff curr_mon poly2) &&
             equal_simplified tail (delete_monomial curr_mon poly2)

    let monomials poly =
         poly
      |> simplify
      |> List.map ScaledMonomial_.monomial
      |> List.filter ((<>) Monomial_.one)

    let from_monomial mon = lift Value.one mon

    let from_power var n = from_monomial (Monomial_.lift var n)
      
    let from_constant c = lift c Monomial_.one

    let from_var var = from_power var 1

    let from_var_string str = from_var (Var.of_string str)

    let from_constant_int c = from_constant (Value.of_int c)
                      
    (* Gets the constant *)
    let constant poly = coeff Monomial_.one (simplify poly)

    let vars poly =
         poly
      |> simplify
      |> monomials
      |> List.map Monomial_.vars
      |> List.fold_left Set.union Set.empty
      
    let is_var poly =
         poly
      |> simplify
      |> monomials
      |> fun monomials -> List.length monomials == 1 &&
                            Monomial_.is_univariate_linear (List.hd monomials) && (Value.equal (coeff (List.hd monomials) poly) Value.one)

    let is_var_plus_constant poly =
         poly
      |> delete_monomial Monomial_.one
      |> is_var

    let is_sum_of_vars_plus_constant poly =
         poly
      |> delete_monomial Monomial_.one
      |> List.for_all (fun scaled -> Value.equal (ScaledMonomial_.coeff scaled) Value.one &&
                                       Monomial_.is_univariate_linear (ScaledMonomial_.monomial scaled))

    let is_univariate_linear poly = 
      degree poly <= 1 && Set.cardinal (vars poly) <= 1

    let is_const poly = degree poly <= 0

    let is_linear poly = (degree poly <= 1)

    let rename varmapping poly =
      List.map (ScaledMonomial_.rename varmapping) poly

    let mult_with_const const poly =
      List.map (ScaledMonomial_.mult_with_const const) poly

    type outer_t = t
    module BaseMathImpl : (PolyTypes.BaseMath with type t = outer_t) =
      struct
        type t = outer_t
               
        let zero = []
                 
        let one = lift Value.one Monomial_.one
                
        let neg poly =
          mult_with_const (Value.neg Value.one) poly
          
        let add poly1 poly2 =
          simplify (List.append poly1 poly2)
          
        let mul poly1 poly2 =
             List.cartesian_product poly1 poly2
          |> List.map (fun (a, b) -> ScaledMonomial_.mul a b) 
           
        let pow poly d =
             poly
          |> Enum.repeat ~times:d
          |> Enum.fold mul one
      
      end
    include PolyTypes.MakeMath(BaseMathImpl)

    module BasePartialOrderImpl : (PolyTypes.BasePartialOrder with type t = outer_t) =
      struct
        type t = outer_t
               
        let (==) poly1 poly2 = 
          equal_simplified (simplify poly1) (simplify poly2)
          
        let (>) p1 p2 = match (p1, p2) with
          (* TODO Find some rules to compare polynomials *)
          | ([s1], [s2]) -> ScaledMonomial_.(>) s1 s2
          | _ -> None
               
      end
    include PolyTypes.MakePartialOrder(BasePartialOrderImpl)

    let is_zero poly = simplify poly == zero

    let is_one poly = simplify poly == one
                     
    let eval poly valuation =
         poly
      |> List.map (fun scaled -> ScaledMonomial_.eval scaled valuation)
      |> List.fold_left Value.add Value.zero

    let replace poly poly_valuation =
      raise (Failure "Replace for Polynomial not yet implemented")
  end
