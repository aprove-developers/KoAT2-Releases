open Mapping
type scaled_mon = 
    {   coeff : Big_int.big_int; 
        mon :   Monomials.t;
    }

type value = Big_int.big_int
    
let mk_scaled_mon_from_mon (coefficient : Big_int.big_int) (monomial : Monomials.t) = { coeff = coefficient; mon = monomial }

let get_coeff (scaled : scaled_mon) =
    scaled.coeff

let get_monom (scaled : scaled_mon) =
    scaled.mon

let get_degree (scaled : scaled_mon) =
    Monomials.get_degree (scaled.mon)

let simplify (scaled : scaled_mon) =
    {   coeff = scaled.coeff ; 
        mon = (Monomials.simplify scaled.mon)
    }

let to_string_simplified (scaled : scaled_mon) =
    if (Big_int.eq_big_int scaled.coeff Big_int.unit_big_int) then (Monomials.to_string scaled.mon)
    else if scaled.mon == Monomials.mk_mon [] then String.concat "" ["(" ; (Big_int.string_of_big_int scaled.coeff) ; ")"] 
    else String.concat "" ["(" ; (Big_int.string_of_big_int scaled.coeff) ; ")" ; "*" ; (Monomials.to_string scaled.mon)]

let to_string (scaled : scaled_mon) = to_string_simplified (simplify scaled)

let to_z3_simplified (ctx:Z3.context) (scaled : scaled_mon) =
   Z3.Arithmetic.mk_mul ctx [(Z3.Arithmetic.Integer.mk_numeral_s ctx (Big_int.string_of_big_int scaled.coeff)) ; (Monomials.to_z3 ctx scaled.mon)]
   
let to_z3 (ctx:Z3.context) (scaled : scaled_mon) =
    to_z3_simplified ctx (simplify scaled)

let equal (scaled1 : scaled_mon) (scaled2 : scaled_mon) =
    (Big_int.eq_big_int scaled1.coeff scaled2.coeff) && (Monomials.equal scaled1.mon scaled2.mon)

let rename_scaled_mon (varmapping : string VarMap.t) (scaled : scaled_mon) =
    {   coeff = scaled.coeff;
        mon = Monomials.rename_monomial varmapping scaled.mon
    }

let eval (varmapping : value VarMap.t) (scaled : scaled_mon) =
    Big_int.mult_big_int scaled.coeff (Monomials.eval varmapping scaled.mon)

let mult_with_const (const : value) (scaled : scaled_mon) =
    {   coeff = Big_int.mult_big_int scaled.coeff const; 
        mon = scaled.mon
    }

let mult (scaled1 : scaled_mon) (scaled2 : scaled_mon) =
    {   coeff = Big_int.mult_big_int scaled1.coeff scaled2.coeff;
        mon = Monomials.mult scaled1.mon scaled2.mon
    }
