open Batteries
open PolyTypes
   
module MakeVariableTerm(Var : ID)(Value : Number.Numeric) =
  struct
    module Valuation = Valuation.MakeValuation(Var)(Value)
    module RenameMap = Map.Make(Var)

    type t = Var.t
    type value = Value.t
    type valuation = Valuation.t
    type var = Var.t
    type rename_map = var RenameMap.t
             
    let of_string = Var.of_string
    let to_string = Var.to_string
    let (==) = Var.(==)
    let compare = Var.compare

    let vars var = [var]
                      
    let eval = Valuation.eval

    let to_z3 ctx var =
      Z3.Arithmetic.Integer.mk_const ctx (Z3.Symbol.mk_string ctx (to_string var))
      
    let rename rename_map var =
      if RenameMap.mem var rename_map then
        RenameMap.find var rename_map
      else var

    let degree var = 1
      
  end
