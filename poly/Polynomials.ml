open Batteries
open ID
open Evaluable
   
module type Polynomial =
  sig
    type t
    type power
    type monomial
    type scaled_monomial
    include Evaluable with type t := t
    val make : scaled_monomial list -> t
    val lift : scaled_monomial -> t
    val coeff : monomial -> t -> value
    val delete_monomial : monomial -> t -> t
    val simplify : t -> t
    val to_string : t -> string
    val monomials : t -> monomial list
    val from_var : var -> t
    val zero : t
    val one : t
    val constant : t -> value
    val from_constant : value -> t
    val is_var : t -> bool
    val is_var_plus_constant : t -> bool
    val is_sum_of_vars_plus_constant : t -> bool
    val is_univariate_linear : t -> bool
    val is_const : t -> bool
    val is_linear : t -> bool
    val mult_with_const : value -> t -> t
    val negate : t -> t
    val add : t -> t -> t
    val sum : t list -> t
    val subtract : t -> t -> t
    val mult : t -> t -> t
    val pow : t -> int -> t
  end

module MakePolynomial(Var : ID) =
  struct
    module VariableTerm = Variables.MakeVariableTerm(Var)
    module Valuation = Valuation.MakeValuation(Var)
    module RenameMap = Map.Make(Var)
    module Power = Powers.MakePower(Var)
    module Monomial = Monomials.MakeMonomial(Var)
    module ScaledMonomial = ScaledMonomials.MakeScaledMonomial(Var)
                          
    type t = ScaledMonomial.t list 
    type value = VariableTerm.value
    type valuation = VariableTerm.valuation
    type var = VariableTerm.t
    type rename_map = var RenameMap.t
    type power = Power.t
    type monomial = Monomial.t
    type scaled_monomial = ScaledMonomial.t

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
      |> List.fold_left Big_int.add Big_int.zero

    let delete_monomial mon poly =
      List.filter (fun x -> not (Monomial.(==) (ScaledMonomial.monomial x) mon)) poly

    let rec simplify_partial_simplified poly =
      match poly with 
      | [] -> []
      | scaled::tail ->
         let curr_monom = ScaledMonomial.monomial scaled in
         let curr_coeff = coeff curr_monom poly in
         if (Big_int.equal curr_coeff Big_int.zero) then (simplify_partial_simplified (delete_monomial curr_monom tail))
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
           Big_int.equal curr_coeff (coeff curr_mon poly2) &&
             equal_simplified tail (delete_monomial curr_mon poly2)

    (* Returns the monomials of a polynomial without the empty monomial *)
    let monomials poly =
         poly
      |> simplify
      |> List.map ScaledMonomial.monomial
      |> List.filter ((<>) Monomial.one)

    (* Returns a variable as a polynomial *)

    let from_var var =
         Power.make var 1
      |> Monomial.lift
      |> ScaledMonomial.make Big_int.one
      |> List.singleton

    (* Return "zero" as a polynomial *)

    let zero = []

    (* Return "one" as a polynomial *)

    let one = 
      [ ScaledMonomial.make Big_int.one Monomial.one ]

    (* Gets the constant *)
    let constant poly = coeff Monomial.one poly

    let from_constant c =
      [(ScaledMonomial.make c Monomial.one)]


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
      |> List.for_all (fun scaled -> Big_int.equal (ScaledMonomial.coeff scaled) Big_int.one &&
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

    let negate poly =
      mult_with_const (Big_int.neg Big_int.one) poly

    (*addition of two polynomials is just concatenation*)

    let add poly1 poly2 =
      simplify (List.append poly1 poly2)

    let sum pollist =
      simplify (List.concat pollist) 

    let subtract poly1 poly2 =
      add poly1 (negate poly2)

    (*multiplication of two polynomials*)

    let mult poly1 poly2 =
         List.cartesian_product poly1 poly2
      |> List.map (fun (a, b) -> ScaledMonomial.mult a b) 

    let pow poly d =
         poly
      |> Enum.repeat ~times:d
      |> Enum.fold mult one
      
    (*instantiates the variables in a polynomial with big ints*)

    let eval poly valuation =
         poly
      |> List.map (fun scaled -> ScaledMonomial.eval scaled valuation)
      |> List.fold_left Big_int.add Big_int.zero

    let (==) poly1 poly2 = 
      equal_simplified (simplify poly1) (simplify poly2)

  end

module StringPolynomial = MakePolynomial(StringID)
