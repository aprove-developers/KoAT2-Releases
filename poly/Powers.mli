open Batteries
open ID
open PolyTypes

module MakePower(Var : ID) : Power with type var = Var.t
                                    and type rename_map = Var.t Map.Make(Var).t
                                    and type value = Big_int.big_int
                                    and type valuation = Valuation.MakeValuation(Var).t


module StringPower : Power
