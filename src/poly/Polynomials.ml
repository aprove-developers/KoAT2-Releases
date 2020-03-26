open Batteries
open PolyTypes
   
module PolynomialOver(Value : PolyTypes.Ring) =
  struct
    module Monomial_ = Monomials.Make(Value)
    module ScaledMonomial_ = ScaledMonomials.Make(Value)
    module Valuation_ = Valuation.Make(Value)
    type valuation = Valuation.Make(Value).t
                      
    type monomial = Monomial_.t
    type scaled_monomial = ScaledMonomial_.t
    type t = ScaledMonomial_.t list [@@deriving eq, ord]
    type value = Value.t

    let make = List.map (fun (coeff, mon) -> ScaledMonomial_.make coeff mon)
    
    let lift coeff mon = [ScaledMonomial_.make coeff mon]

    let of_scaled scaled = scaled

    let fold ~const ~var ~neg ~plus ~times ~pow =
      List.fold_left (fun b scaled -> plus b (ScaledMonomial_.fold ~const ~var ~times ~pow scaled)) (const Value.zero)
                    
    let degree poly =
      List.max (List.map (ScaledMonomial_.degree) poly )
      
    let coeff mon poly =
         poly
      |> List.filter (fun scaled -> Monomial_.(=~=) (ScaledMonomial_.monomial scaled) mon)
      |> List.map ScaledMonomial_.coeff
      |> List.fold_left Value.add Value.zero
      
    let coeff_of_var var poly =
        let mon = Monomial_.lift var 1 in 
            coeff mon poly

    let delete_monomial mon poly =
      List.filter (fun x -> not (Monomial_.(=~=) (ScaledMonomial_.monomial x) mon)) poly

    let rec simplify poly =
      match poly with 
      | [] -> []
      | scaled::tail ->
         let curr_monom = ScaledMonomial_.monomial scaled in
         let curr_coeff = coeff curr_monom poly in
         if Value.(curr_coeff =~= zero) then (simplify (delete_monomial curr_monom tail))
         else (ScaledMonomial_.make curr_coeff curr_monom) :: (simplify (delete_monomial curr_monom tail) )

    let to_string_simplified ?(to_file=false) = function 
      | [] -> "0"
      | poly -> let str = String.concat "" (List.map (ScaledMonomial_.to_string ~to_file) poly) in
                if String.starts_with str "+" then
                  String.lchop str else str

    let to_string poly = to_string_simplified (simplify poly)

    let to_string_to_file poly = to_string_simplified ~to_file:true (simplify poly)

    let rec equal_simplified poly1 poly2 =
      List.length poly1 == List.length poly2 &&
        match poly1 with
        | [] -> true
        | scaled :: tail ->
           let curr_mon = ScaledMonomial_.monomial scaled in
           let curr_coeff = ScaledMonomial_.coeff scaled in
           Value.(=~=) curr_coeff (coeff curr_mon poly2) &&
             equal_simplified tail (delete_monomial curr_mon poly2)

    let monomials poly =
         poly
      |> simplify
      |> List.map ScaledMonomial_.monomial
      |> List.filter ((<>) Monomial_.one)

    let of_monomial mon = lift Value.one mon

    let of_power var n = of_monomial (Monomial_.lift var n)
      
    let of_constant c = lift c Monomial_.one

    let of_var var = of_power var 1
    
    let rec of_coeff_list coeffs vars =
        if (List.length coeffs) == (List.length vars) then
            match (coeffs, vars) with
                |([],[])-> []
                |(c::coefftail, v::varstail)-> (ScaledMonomial_.make c (Monomial_.lift v 1)) :: (of_coeff_list coefftail varstail)
                |_ -> []
        else []
            

    let var str = of_var (Var.of_string str)

    let value c = of_constant (Value.of_int c)
    
    let real_helper n = of_var (Var.mk_helper Var.Real n)

    let int_helper n = of_var (Var.mk_helper Var.Int n)
    
    let of_int = value                        

    let to_int poly = raise (Failure "TODO: Not possible")
                
    (* Gets the constant *)
    let get_constant poly = coeff Monomial_.one (simplify poly)

    let vars poly =
         poly
      |> simplify
      |> monomials
      |> List.map Monomial_.vars
      |> List.fold_left VarSet.union VarSet.empty
      
    let is_var poly =
         poly
      |> simplify
      |> monomials
      |> fun monomials -> List.length monomials == 1 &&
                            Monomial_.is_univariate_linear (List.hd monomials) && (Value.(=~=) (coeff (List.hd monomials) poly) Value.one)

    let is_var_plus_constant poly =
         poly
      |> delete_monomial Monomial_.one
      |> is_var

    let is_sum_of_vars_plus_constant poly =
         poly
      |> delete_monomial Monomial_.one
      |> List.for_all (fun scaled -> Value.(=~=) (ScaledMonomial_.coeff scaled) Value.one &&
                                       Monomial_.is_univariate_linear (ScaledMonomial_.monomial scaled))

    let is_univariate_linear poly = 
      degree poly <= 1 && VarSet.cardinal (vars poly) <= 1

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
               
        let rec equal_simplified poly1 poly2 =
          List.length poly1 == List.length poly2 &&
            match poly1 with
            | [] -> true
            | scaled :: tail ->
               let curr_mon = ScaledMonomial_.monomial scaled in
               let curr_coeff = ScaledMonomial_.coeff scaled in
               Value.(=~=) curr_coeff (coeff curr_mon poly2) &&
                 equal_simplified tail (delete_monomial curr_mon poly2)
               
        let (=~=) poly1 poly2 = 
          equal_simplified (simplify poly1) (simplify poly2)

        let (>) p1 p2 = match (p1, p2) with
          (* TODO Find some rules to compare polynomials *)
          | ([s1], [s2]) -> ScaledMonomial_.(>) s1 s2
          | _ -> None
               
      end
    include PolyTypes.MakePartialOrder(BasePartialOrderImpl)

          (*
    (* Helper: Returns the greatest common divisor of the two values. Uses the Euclid algorithm. *)
    let rec gcd (a: Value.t) (b: Value.t) =
      if Value.(b =~= zero) then a
      else gcd b (Value.modulo a b)
          
    (* Helper: Returns the greatest common divisor of all coefficients. *)
    let coefficients_gcd (poly: t) =
      List.fold_right gcd (List.map ScaledMonomial_.coeff poly) Value.zero
          
    let scale_coefficients poly =
      let divisor: Value.t = Value.abs (coefficients_gcd poly) in
      let scale_coeff scaled = ScaledMonomial_.(make Value.(coeff scaled / divisor) (monomial scaled)) in
      List.map scale_coeff poly
           *)

    let is_zero poly = poly =~= zero

    let is_one poly = poly =~= one
                    
    let instantiate (substitution : Value.t -> t) =
      fold ~const:substitution ~var:of_var ~neg:neg ~plus:add ~times:mul ~pow:pow

    let eval_f poly f =
         poly
      |> List.map (fun scaled -> ScaledMonomial_.eval_f scaled f)
      |> List.fold_left Value.add Value.zero      
      
    let eval poly valuation =
         poly
      |> List.map (fun scaled -> ScaledMonomial_.eval scaled valuation)
      |> List.fold_left Value.add Value.zero

    let substitute_f substitution =
      fold ~const:of_constant ~var:substitution ~neg:neg ~plus:add ~times:mul ~pow:pow
          
    let substitute var ~replacement =
      substitute_f (fun target_var ->
          if Var.(var =~= target_var) then replacement else of_var target_var
        )

    let substitute_all substitution =
      let module VarMap = Map.Make(Var) in
      substitute_f (fun var ->
          VarMap.find_default (of_var var) var substitution
        )
      
    let eval_partial poly valuation =
      substitute_f (fun var ->
          Option.map of_constant (Valuation_.eval_opt var valuation) |? of_var var
        ) poly

    let partition =
      List.partition
      
  end


module Polynomial =
  struct
    include PolynomialOver(OurInt)

    let separate_by_sign poly =
      partition (fun scaled -> OurInt.Compare.(ScaledMonomial_.coeff scaled >= OurInt.zero)) poly

    let max_of_occurring_constants =
      fold
        ~const:OurInt.abs
        ~var:(fun _ -> OurInt.one)
        ~neg:identity
        ~plus:OurInt.add
        ~times:OurInt.mul
        ~pow:OurInt.pow
      
  end


module ParameterPolynomial =
  struct
    module Outer = PolynomialOver(PolynomialOver(OurInt))
    module Inner = PolynomialOver(OurInt)
                 
    include Outer
          
    let eval_coefficients (f: Var.t -> OurInt.t) =
      Outer.fold ~const:(fun inner -> Inner.of_constant (Inner.eval_f inner f))
                 ~var:Inner.of_var
                 ~neg:Inner.neg
                 ~plus:Inner.add
                 ~times:Inner.mul
                 ~pow:Inner.pow
 
    (** Transforms the template polynomial such that all inner values get lifted to the outer polynomial. *)
    (** Example: (2a+b)x + (3a)y - 1 gets transformed to 2ax + bx + 3ay - 1 *)
    let flatten (templatepoly : Outer.t): Inner.t =
      Outer.fold ~const:identity ~var:Inner.of_var ~neg:Inner.neg ~plus:Inner.add ~times:Inner.mul ~pow:Inner.pow templatepoly
      
    (** Lifts a polynomial to a parameter polynomial such that the inner structure is kept.*)
    (** Example: 2x +3 is interpreted as 2x+3 and not as the constant polynomial (2x+3)*(1)*)
    let of_polynomial (poly : Inner.t): t =
      Inner.fold ~const:(fun value -> of_constant (Inner.of_constant value)) ~var:of_var ~neg:neg ~plus:add ~times:mul ~pow:pow poly
  end