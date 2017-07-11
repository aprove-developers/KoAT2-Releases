open Batteries
open ID
open PolyTypes

module MakeScaledMonomial(Var : ID) : ScaledMonomial with type var = Var.t
                                                      and type rename_map = Var.t Map.Make(Var).t
                                                      and type value = Big_int.big_int
                                                      and type valuation = Valuation.MakeValuation(Var).t
                                                      and type power = Powers.MakePower(Var).t
                                                      and type monomial = Monomials.MakeMonomial(Var).t


module StringScaledMonomial : ScaledMonomial

