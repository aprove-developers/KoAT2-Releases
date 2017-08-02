open Batteries

module Make
         (Var : PolyTypes.ID)
       : PolyTypes.RenameMap with type var = Var.t
