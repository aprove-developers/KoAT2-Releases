open ID
open Evaluable
   
module MakeVariableTerm(Var : ID) : Evaluable with type var = Var.t
                                               and type t = Var.t
                                               and type rename_map = Var.t Map.Make(Var).t

module StringVariableTerm : Evaluable with type var = StringID.t
                                       and type value = Big_int.big_int
                                       and type rename_map = StringID.t Map.Make(StringID).t
                                       and type valuation = Valuation.MakeValuation(StringID).t
