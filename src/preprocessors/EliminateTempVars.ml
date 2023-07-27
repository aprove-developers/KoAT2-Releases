(** Implemenation of a preprocessor which eliminates tmp variables u(x) = T :|: T <= p(x) && T >= p(x). This is in particular useful for twn-loops. *)
open Batteries
open Formulas
open Constraints
open Atoms
open Polynomials
open ProgramModules

let logger = Logging.(get Preprocessor)

let eliminate_tmp_vars program =
    let tmp_vars = VarSet.to_list @@ Program.tmp_vars program and
    trans = TransitionSet.to_list @@ Program.transitions program in
    let result =
        MaybeChanged.fold (fun trans (l,t,l') ->
            let changed_t = MaybeChanged.fold (flip TransitionLabel.eliminate_tmp_var) t tmp_vars in
            if MaybeChanged.has_changed changed_t then (
                Logger.(log logger INFO (fun () -> "EliminateTempVars", [("new label", TransitionLabel.to_string @@ MaybeChanged.unpack changed_t)]));
                MaybeChanged.changed (TransitionSet.add (l,MaybeChanged.unpack changed_t,l') trans))
            else
                MaybeChanged.same (TransitionSet.add (l,t,l') trans)
        )
        TransitionSet.empty trans
    in
    if MaybeChanged.has_changed result then
        MaybeChanged.changed @@
            Program.from_enum (Program.start program) (TransitionSet.enum @@ MaybeChanged.unpack result)
    else
        MaybeChanged.same program


