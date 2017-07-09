open ID
open Evaluable

module type Power =
  sig
    type t
    include Evaluable with type t := t
    val make : var -> int -> t
    val var : t -> var
    val n : t -> int
  end

module MakePower(Var : ID) : Power with type var = Var.t
                                    and type rename_map = Var.t Map.Make(Var).t
                                    and type value = Big_int.big_int
                                    and type valuation = Valuation.MakeValuation(Var).t


module StringPower : Power
