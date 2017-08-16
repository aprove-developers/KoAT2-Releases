open Batteries

(** Constructs a rename map with the given variable type *)
module Make
         (Var : PolyTypes.ID)
       : PolyTypes.RenameMap with type var = Var.t
