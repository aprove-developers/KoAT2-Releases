open Batteries
open ID
open PolyTypes

module MakePolynomial(Var : ID) : Polynomial with type var = Var.t
                                              and type rename_map = Var.t Map.Make(Var).t
                                              and type value = Big_int.big_int
                                              and type valuation = Valuation.MakeValuation(Var).t
                                              and type power = Powers.MakePower(Var).t
                                              and type monomial = Monomials.MakeMonomial(Var).t
                                              and type scaled_monomial = ScaledMonomials.MakeScaledMonomial(Var).t


module StringPolynomial : Polynomial
