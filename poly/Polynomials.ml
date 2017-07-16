open Batteries
open PolyTypes
   
module MakePolynomial(Var : ID)(Value : Number.Numeric) =
  struct
    module VariableTerm = Variables.MakeVariableTerm(Var)(Value)
    module Valuation = Valuation.MakeValuation(Var)(Value)
    module RenameMap = Map.Make(Var)
    module Power = Powers.MakePower(Var)(Value)
    module Monomial = Monomials.MakeMonomial(Var)(Value)
    module ScaledMonomial = ScaledMonomials.MakeScaledMonomial(Var)(Value)
    module PolynomialAST = PolynomialAST(Var)
    module PolyValuation = Valuation (* TODO Problem with self reference: Valuation.MakeValuation(Var)(MakePolynomial(Var)(Value)) *)
                          
    type t = ScaledMonomial.t list 
    type value = Value.t
    type valuation = Valuation.t
    type var = Var.t
    type rename_map = var RenameMap.t
    type power = Power.t
    type monomial = Monomial.t
    type scaled_monomial = ScaledMonomial.t
    type polynomial_ast = PolynomialAST.t
    type poly_valuation = PolyValuation.t
                        
    let make scaleds = scaleds

    let lift scaled = [scaled]

    let compare a b = 0 (* TODO *)
                         
    let degree poly =
      List.max (List.map (ScaledMonomial.degree) poly )
      
    (* Returns the coefficient of a monomial *)
    let coeff mon poly =
         poly
      |> List.filter (fun scaled -> Monomial.(==) (ScaledMonomial.monomial scaled) mon)
      |> List.map ScaledMonomial.coeff
      |> List.fold_left Value.add Value.zero

    let delete_monomial mon poly =
      List.filter (fun x -> not (Monomial.(==) (ScaledMonomial.monomial x) mon)) poly

    let rec simplify_partial_simplified poly =
      match poly with 
      | [] -> []
      | scaled::tail ->
         let curr_monom = ScaledMonomial.monomial scaled in
         let curr_coeff = coeff curr_monom poly in
         if (Value.equal curr_coeff Value.zero) then (simplify_partial_simplified (delete_monomial curr_monom tail))
         else (ScaledMonomial.make curr_coeff curr_monom) :: (simplify_partial_simplified (delete_monomial curr_monom tail) )

    let simplify poly =
      simplify_partial_simplified (List.map (ScaledMonomial.simplify) poly)

    let to_string_simplified poly = 
      if (poly == []) then "0" 
      else 
        String.concat "+" (List.map ScaledMonomial.to_string poly)

    let to_string poly = to_string_simplified (simplify poly)

    let of_string poly = raise (Failure "Not yet implemented") (* TODO Use ocamlyacc *)

    let to_z3_simplified ctx poly = 
      if poly == [] then (Z3.Arithmetic.Integer.mk_numeral_i ctx 0)
      else    Z3.Arithmetic.mk_add ctx  (List.map (ScaledMonomial.to_z3 ctx) poly)
      
    let to_z3 ctx poly = 
      to_z3_simplified ctx (simplify poly)

    let rec equal_simplified poly1 poly2 =
      List.length poly1 == List.length poly2 &&
        match poly1 with
        | [] -> true
        | scaled :: tail ->
           let curr_mon = ScaledMonomial.monomial scaled in
           let curr_coeff = ScaledMonomial.coeff scaled in
           Value.equal curr_coeff (coeff curr_mon poly2) &&
             equal_simplified tail (delete_monomial curr_mon poly2)

    (* Returns the monomials of a polynomial without the empty monomial *)
    let monomials poly =
         poly
      |> simplify
      |> List.map ScaledMonomial.monomial
      |> List.filter ((<>) Monomial.one)

    (* Returns a variable as a polynomial *)

    let from_scaled_monomial = lift

    let from_monomial mon = from_scaled_monomial (ScaledMonomial.lift mon)

    let from_power power = from_monomial (Monomial.lift power)
      
    let from_constant c = lift (ScaledMonomial.make c Monomial.one)

    let from_var var = from_power (Power.lift var)

    (* Return "zero" as a polynomial *)

    let zero = []

    (* Return "one" as a polynomial *)

    let one = lift ScaledMonomial.one

    (* Gets the constant *)
    let constant poly = coeff Monomial.one poly

    (* Returns the variables of a polynomial *)          
    let vars poly =
         poly
      |> simplify
      |> monomials
      |> List.map Monomial.vars
      |> List.concat
      |> List.unique
      
    (* Checks whether a polynomial is a single variable *)
    let is_var poly =
         poly
      |> simplify
      |> monomials
      |> fun monomials -> List.length monomials == 1 &&
                            Monomial.is_univariate_linear (List.hd monomials)

    (* Checks wheather a polynomial is a single variable plus a constant*)
    let is_var_plus_constant poly =
         poly
      |> delete_monomial Monomial.one
      |> is_var

    (* Checks whether a polynomial is a sum of variables plus a constant *)
    let is_sum_of_vars_plus_constant poly =
         poly
      |> delete_monomial Monomial.one
      |> List.for_all (fun scaled -> Value.equal (ScaledMonomial.coeff scaled) Value.one &&
                                       Monomial.is_univariate_linear (ScaledMonomial.monomial scaled))

    (* Checks whether a polynomial is a sum of variables plus a constant *)
    let is_sum_of_vars_plus_constant poly =
      degree poly == 1

    (* Checks whether a polyomial is linear and contains just one active variable*)
    let is_univariate_linear poly = 
      degree poly == 1 && List.length (vars poly) == 1

    let is_const poly = degree poly <= 0

    let is_linear = is_sum_of_vars_plus_constant 

    (*renames the variables occuring inside a polynomial*) 

    let rename varmapping poly =
      List.map (ScaledMonomial.rename varmapping) poly

    (*multiply a polynomial by a constant*)

    let mult_with_const const poly =
      List.map (ScaledMonomial.mult_with_const const) poly

    let neg poly =
      mult_with_const (Value.neg Value.one) poly

    (*addition of two polynomials is just concatenation*)

    let add poly1 poly2 =
      simplify (List.append poly1 poly2)

    let sum pollist =
      simplify (List.concat pollist) 

    let sub poly1 poly2 =
      add poly1 (neg poly2)

    (*multiplication of two polynomials*)

    let mul poly1 poly2 =
         List.cartesian_product poly1 poly2
      |> List.map (fun (a, b) -> ScaledMonomial.mul a b) 

    let pow poly d =
         poly
      |> Enum.repeat ~times:d
      |> Enum.fold mul one
      
    (*instantiates the variables in a polynomial with big ints*)

    let eval poly valuation =
         poly
      |> List.map (fun scaled -> ScaledMonomial.eval scaled valuation)
      |> List.fold_left Value.add Value.zero

    let (==) poly1 poly2 = 
      equal_simplified (simplify poly1) (simplify poly2)

    let rec from_ast ast = match ast with
      | PolynomialAST.Constant c -> from_constant (Value.of_int c)
      | PolynomialAST.Variable v -> from_var v
      | PolynomialAST.Neg t -> neg (from_ast t)
      | PolynomialAST.Plus (t1, t2) -> add (from_ast t1) (from_ast t2)
      | PolynomialAST.Times (t1, t2) -> mul (from_ast t1) (from_ast t2)
      | PolynomialAST.Pow (var, n) -> from_power (Power.make var n)

    let replace poly poly_valuation =
      raise (Failure "Not yet implemented")
                                    
  end
