open Batteries
   
module MakePower(Var : PolyTypes.ID)(Value : Number.Numeric) =
  struct
    module Valuation_ = Valuation.MakeValuation(Var)(Value)
    module RenameMap_ = RenameMap.MakeRenameMap(Var)

    type t = {
        var : Var.t;
        n : int
      }

    module Var = Var
    module Value = Value

    let fold ~const ~var ~times ~pow power =
      pow (var power.var) power.n

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
      else Value.pow (Valuation_.eval power.var valuation) (Value.of_int power.n)
               
    let rename varmapping power = {
        power with var = RenameMap_.find power.var varmapping power.var
      }

    let make var n = {
        var = var;
        n = n
      }

    let lift var = make var 1

    let data power = (power.var, power.n)
                 
    let var t = t.var
                   
    let n t = t.n

    let degree = n

  end
