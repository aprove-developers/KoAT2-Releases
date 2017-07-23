open Batteries

module MakeRenameMap
         (Var : PolyTypes.ID)
       : PolyTypes.RenameMap with type var = Var.t
