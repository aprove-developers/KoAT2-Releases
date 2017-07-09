open Batteries
open ID
   
module VariableTerm = Variables.StringVariableTerm
module Monomial = Monomials.MakeMonomial(StringID)

type valuation = VariableTerm.valuation
type t = 
    {
        coeff : Big_int.big_int; 
        mon :   Monomial.t;
    }
type value = VariableTerm.value
    
let mk_scaled_mon_from_mon coefficient monomial = { coeff = coefficient; mon = monomial }

let get_coeff scaled = scaled.coeff

let get_monom scaled = scaled.mon

let get_degree scaled = Monomial.degree scaled.mon

let simplify scaled =
    {
        coeff = scaled.coeff ; 
        mon = (Monomial.simplify scaled.mon)
    }

let to_string_simplified scaled =
    if (Big_int.eq_big_int scaled.coeff Big_int.unit_big_int) then (Monomial.to_string scaled.mon)
    else if scaled.mon == Monomial.make [] then String.concat "" ["(" ; (Big_int.string_of_big_int scaled.coeff) ; ")"] 
    else String.concat "" ["(" ; (Big_int.string_of_big_int scaled.coeff) ; ")" ; "*" ; (Monomial.to_string scaled.mon)]

let to_string scaled = to_string_simplified (simplify scaled)

let to_z3_simplified ctx scaled =
   Z3.Arithmetic.mk_mul ctx [(Z3.Arithmetic.Integer.mk_numeral_s ctx (Big_int.string_of_big_int scaled.coeff)) ; (Monomial.to_z3 ctx scaled.mon)]
   
let to_z3 ctx scaled =
    to_z3_simplified ctx (simplify scaled)

let equal scaled1 scaled2 =
    (Big_int.eq_big_int scaled1.coeff scaled2.coeff) && (Monomial.(==) scaled1.mon scaled2.mon)

let rename_scaled_mon varmapping scaled =
    {
        coeff = scaled.coeff;
        mon = Monomial.rename varmapping scaled.mon
    }

let eval varmapping scaled =
    Big_int.mult_big_int scaled.coeff (Monomial.eval scaled.mon varmapping)

let mult_with_const const scaled =
    {
        coeff = Big_int.mult_big_int scaled.coeff const; 
        mon = scaled.mon
    }

let mult scaled1 scaled2 =
    {
        coeff = Big_int.mult_big_int scaled1.coeff scaled2.coeff;
        mon = Monomial.mult scaled1.mon scaled2.mon
    }
