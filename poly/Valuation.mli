open Batteries
open ID
open PolyTypes

module MakeValuation(Id : ID) : Valuation with type var = Id.t
                                           and type value = Big_int.big_int

module StringValuation : Valuation with type var = StringID.t
                                    and type value = Big_int.big_int
