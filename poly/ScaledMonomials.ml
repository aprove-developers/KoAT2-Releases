open Batteries
open Big_int.Infix

module MakeScaledMonomial(Var : PolyTypes.ID)(Value : Number.Numeric) =
  struct    
    module Valuation_ = Valuation.MakeValuation(Var)(Value)
    module RenameMap_ = RenameMap.MakeRenameMap(Var)
    module Power = Powers.MakePower(Var)(Value)
    module Monomial = Monomials.MakeMonomial(Var)(Value)

    type t = 
      {
        coeff : Value.t; 
        mon :   Monomial.t;
      }    
    type power = Power.t
    type monomial = Monomial.t

    module Var = Var
    module Value = Value
               
    let make coefficient monomial = { coeff = coefficient; mon = monomial }
                                  
    let coeff scaled = scaled.coeff
                     
    let monomial scaled = scaled.mon
                        
    let degree scaled = Monomial.degree scaled.mon
                      
    let simplify scaled = { scaled with mon = Monomial.simplify scaled.mon }

    let to_string_simplified scaled =
      if scaled.coeff == Value.one then Monomial.to_string scaled.mon
      else if scaled.mon == Monomial.one then String.concat "" ["(" ; (Value.to_string scaled.coeff) ; ")"] 
      else String.concat "" ["(" ; (Value.to_string scaled.coeff) ; ")" ; "*" ; (Monomial.to_string scaled.mon)]
      
    let to_string scaled = to_string_simplified (simplify scaled)
                         
    let of_string str = raise (Failure "Not implemented") (* TODO Use ocamlyacc here *)

    let to_z3_simplified ctx scaled =
      Z3.Arithmetic.mk_mul ctx [(Z3.Arithmetic.Integer.mk_numeral_s ctx (Value.to_string scaled.coeff)) ; (Monomial.to_z3 ctx scaled.mon)]
      
    let to_z3 ctx scaled =
      to_z3_simplified ctx (simplify scaled)
      
    type outer_t = t
    module BasePartialOrderImpl : (PolyTypes.BasePartialOrder with type t = outer_t) =
      struct
        type t = outer_t
               
        let (==) scaled1 scaled2 =
          (scaled1.coeff == scaled2.coeff) && (Monomial.(==) scaled1.mon scaled2.mon)
          
        let (>) s1 s2 = match (s1, s2) with
          (* TODO Find some rules to compare *)
          | (s1, s2) -> if (Monomial.(==) (monomial s1) (monomial s2) && (coeff s1 > coeff s2)) then
                          Some true
                        else None
                      
      end
    include PolyTypes.MakePartialOrder(BasePartialOrderImpl)

    let rename varmapping scaled = { scaled with mon = Monomial.rename varmapping scaled.mon }
                                 
    let eval scaled valuation =
      Value.mul scaled.coeff (Monomial.eval scaled.mon valuation)
      
    let mult_with_const const scaled = { scaled with coeff = Value.mul scaled.coeff const }
                                     
    let mul scaled1 scaled2 =
      {
        coeff = Value.mul scaled1.coeff scaled2.coeff;
        mon = Monomial.mul scaled1.mon scaled2.mon
      }

    let one =
      {
        coeff = Value.one;
        mon = Monomial.one
      }
      
    let lift mon =
      {
        coeff = Value.one;
        mon = mon
      }

    let vars scaled = Monomial.vars scaled.mon

  end
