open Mapping
type scaled_mon = Scaled of (Big_int.big_int * Monomials.monomial)
    
let mk_scaled_mon_from_mon (coeff : Big_int.big_int) (mon:Monomials.monomial) = Scaled (coeff,mon)

let to_z3 (ctx:Z3.context) (scaled : scaled_mon) =
    match scaled with
        | Scaled (coeff, mon) -> Z3.Arithmetic.mk_mul ctx [(Z3.Arithmetic.Integer.mk_numeral_s ctx (Big_int.string_of_big_int coeff)) ; (Monomials.to_z3 ctx mon)]

let get_coeff (scaled : scaled_mon) =
    match scaled with
        | Scaled (coeff, mon) -> coeff

let get_monom (scaled : scaled_mon) =
    match scaled with
        | Scaled (coeff, mon) -> mon

let get_degree (scaled : scaled_mon) =
    Monomials.get_degree (get_monom scaled)

let simplify (scaled : scaled_mon) =
    match scaled with
        | Scaled (coeff, mon)-> Scaled (coeff, (Monomials.simplify mon))

let to_string_simplified (scaled : scaled_mon) =
    match scaled with
        | Scaled (coeff, mon)-> 
            if (Big_int.eq_big_int coeff Big_int.unit_big_int) then (Monomials.to_string mon)
            else if mon == [] then String.concat "" ["(" ; (Big_int.string_of_big_int coeff) ; ")"] 
            else String.concat "" ["(" ; (Big_int.string_of_big_int coeff) ; ")" ; "*" ; (Monomials.to_string mon)]

let to_string (scaled : scaled_mon) = to_string_simplified (simplify scaled)

let equal (scaled1 : scaled_mon) (scaled2 : scaled_mon) =
    match (scaled1, scaled2) with
        |(Scaled (coeff1, mon1), Scaled (coeff2, mon2)) -> (Big_int.eq_big_int coeff1 coeff2) && (Monomials.equal mon1 mon2)

let rename_scaled_mon (varmapping : string VarMap.t) (scaled : scaled_mon) =
    match scaled with
        |Scaled(coeff, mon) ->  Scaled(coeff, (Monomials.rename_monomial varmapping mon))

let instantiate_with_big_int (varmapping : Big_int.big_int VarMap.t) (scaled : scaled_mon) =
    match scaled with
        |Scaled(coeff, mon) ->  Big_int.mult_big_int coeff (Monomials.instantiate_with_big_int varmapping mon)

let mult_with_const (const : Big_int.big_int) (scaled : scaled_mon) =
    match scaled with
        |Scaled(coeff, mon) -> Scaled((Big_int.mult_big_int coeff const), mon)

let mult (scaled1 : scaled_mon) (scaled2 : scaled_mon) =
    match (scaled1, scaled2) with
        |(Scaled(coeff1, mon1), Scaled(coeff2, mon2)) -> Scaled((Big_int.mult_big_int coeff1 coeff2), (Monomials.mult mon1 mon2))
