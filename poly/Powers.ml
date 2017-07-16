open Batteries
open PolyTypes
   
module MakePower(Var : ID)(Value : Number.Numeric) =
  struct
    module Valuation = Valuation.MakeValuation(Var)(Value)
    module RenameMap = Map.Make(Var)

    type t = {
        var : Var.t;
        n : int
      }
    type value = Value.t
    type valuation = Valuation.t
    type var = Var.t
    type rename_map = var RenameMap.t
             
    let of_string name = {
        var = Var.of_string name;
        n = 1
      }

    let to_string power =
      if power.n <= 0 then "1"
      else if power.n == 1 then (Var.to_string power.var) 
      else String.concat "^" [(Var.to_string power.var); (string_of_int power.n)]

    let (==) power1 power2 =
      (power1.var == power2.var) && ( power1.n == power2.n)

    let vars power = [power.var]

    let eval power valuation =
      if power.n < 0 then Value.zero
      else Value.pow (Valuation.eval power.var valuation) (Value.of_int power.n)
               
    let to_z3 ctx power =
      Z3.Arithmetic.mk_power ctx ( Z3.Arithmetic.Integer.mk_const ctx (Z3.Symbol.mk_string ctx (Var.to_string power.var)) ) (Z3.Arithmetic.Integer.mk_numeral_i ctx power.n)
          
    let rename varmapping power = {
        power with var =
                     if RenameMap.mem power.var varmapping then
                       RenameMap.find power.var varmapping
                     else power.var
      }

    let make var n = {
        var = var;
        n = n
      }

    let lift var = make var 1

    let var t = t.var
                   
    let n t = t.n

    let degree = n

  end
