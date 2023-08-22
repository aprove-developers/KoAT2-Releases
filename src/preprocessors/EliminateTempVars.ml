open! OurBase
open ProgramModules

(** Implemenation of a preprocessor which eliminates tmp variables u(x) = T :|: T <= p(x) && T >= p(x). This is in particular useful for twn-loops. *)

let logger = Logging.(get Preprocessor)

let eliminate_tmp_vars program =
  let tmp_vars = Set.to_list @@ Program.tmp_vars program
  and trans = Set.to_list @@ Program.transitions program in
  let result =
    MaybeChanged.fold
      (fun trans (l, t, l') ->
        let changed_t = MaybeChanged.fold (flip TransitionLabel.eliminate_tmp_var) t tmp_vars in
        if MaybeChanged.has_changed changed_t then (
          Logger.(
            log logger INFO (fun () ->
                ( "EliminateTempVars",
                  [ ("new label", TransitionLabel.to_string @@ MaybeChanged.unpack changed_t) ] )));
          MaybeChanged.changed (Set.add trans (l, MaybeChanged.unpack changed_t, l')))
        else
          MaybeChanged.same (Set.add trans (l, t, l')))
      TransitionSet.empty trans
  in
  if MaybeChanged.has_changed result then
    MaybeChanged.changed
    @@ Program.from_sequence (Program.start program) (Set.to_sequence @@ MaybeChanged.unpack result)
  else
    MaybeChanged.same program
