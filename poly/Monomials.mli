open Batteries
open ID
open PolyTypes
   
module MakeMonomial(Var : ID) : Monomial with type var = Var.t
                                          and type rename_map = Var.t Map.Make(Var).t
                                          and type value = Big_int.big_int
                                          and type valuation = Valuation.MakeValuation(Var).t
                                          and type power = Powers.MakePower(Var).t


module StringMonomial : Monomial

