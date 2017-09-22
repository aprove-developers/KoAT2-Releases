open Batteries

(** Provides default modules to create locations, transitions and transitionsystems *)

module Make(P : PolyTypes.Polynomial) : ProgramTypes.Program
       with module Polynomial_ = P
        and module Atom_ = Atoms.Make(P)
        and module Constraint_ = Constraints.Make(P)
        and module Formula_ = Formula.Make(P)
