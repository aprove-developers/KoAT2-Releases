open Mapping
type t = 
    {
        coeff : Big_int.big_int; 
        mon :   Monomials.t;
    }

type value = Big_int.big_int
    
let mk_scaled_mon_from_mon coefficient monomial = { coeff = coefficient; mon = monomial }

let get_coeff scaled = scaled.coeff

let get_monom scaled = scaled.mon

let get_degree scaled = Monomials.get_degree (scaled.mon)

let simplify scaled =
    {
        coeff = scaled.coeff ; 
        mon = (Monomials.simplify scaled.mon)
    }

let to_string_simplified scaled =
    if (Big_int.eq_big_int scaled.coeff Big_int.unit_big_int) then (Monomials.to_string scaled.mon)
    else if scaled.mon == Monomials.mk_mon [] then String.concat "" ["(" ; (Big_int.string_of_big_int scaled.coeff) ; ")"] 
    else String.concat "" ["(" ; (Big_int.string_of_big_int scaled.coeff) ; ")" ; "*" ; (Monomials.to_string scaled.mon)]

let to_string scaled = to_string_simplified (simplify scaled)

let to_z3_simplified ctx scaled =
   Z3.Arithmetic.mk_mul ctx [(Z3.Arithmetic.Integer.mk_numeral_s ctx (Big_int.string_of_big_int scaled.coeff)) ; (Monomials.to_z3 ctx scaled.mon)]
   
let to_z3 ctx scaled =
    to_z3_simplified ctx (simplify scaled)

let equal scaled1 scaled2 =
    (Big_int.eq_big_int scaled1.coeff scaled2.coeff) && (Monomials.equal scaled1.mon scaled2.mon)

let rename_scaled_mon varmapping scaled =
    {
        coeff = scaled.coeff;
        mon = Monomials.rename_monomial varmapping scaled.mon
    }

let eval varmapping scaled =
    Big_int.mult_big_int scaled.coeff (Monomials.eval varmapping scaled.mon)

let mult_with_const const scaled =
    {
        coeff = Big_int.mult_big_int scaled.coeff const; 
        mon = scaled.mon
    }

let mult scaled1 scaled2 =
    {
        coeff = Big_int.mult_big_int scaled1.coeff scaled2.coeff;
        mon = Monomials.mult scaled1.mon scaled2.mon
    }
