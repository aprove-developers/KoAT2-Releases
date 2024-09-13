open! OurBase
open ProgramModules

let logger = Logging.(get Preprocessor)

(** Implemenation of a preprocessor which eliminates tmp variables u(x) = T :|: T <= p(x) && T >= p(x). This is in particular useful for twn-loops. *)
let eliminate_tmp_vars program =
  let trans = Set.to_list @@ Program.transitions program in
  let result =
    MaybeChanged.fold
      (fun trans (l, t, l') ->
        let changed_t =
          MaybeChanged.fold
            (flip TransitionLabel.eliminate_tmp_var)
            t
            (Set.to_list @@ TransitionLabel.tmp_vars_update t)
        in
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


(** Implemenation of a preprocessor which eliminates tmp variables u(x) = T where x never occurs in an update. This is in particular useful for twn-loops. *)
let tmp_vars_to_nondet program =
  let trans = Set.to_list @@ Program.transitions program in
  let unnecessary_vars =
    Set.filter (Program.input_vars program) ~f:(fun x ->
        List.exists trans ~f:(fun (_, t, _) ->
            let update_x = TransitionLabel.update t x in
            if Option.is_none update_x then
              false
            else
              let update_x_opt = Option.value_exn update_x in
              (not @@ Set.is_empty (UpdateElement.vars update_x_opt))
              && Set.is_subset (UpdateElement.vars update_x_opt)
                   ~of_:(Set.diff (TransitionLabel.tmp_vars t) (Guard.vars (TransitionLabel.guard t))))
        && List.for_all trans ~f:(fun (_, t, _) ->
               let update_x = TransitionLabel.update t x in
               if Option.is_none update_x then
                 true
               else
                 let update_x_opt = Option.value_exn update_x in
                 ((* update: x' = x *)
                  (UpdateElement.is_indeterminate update_x_opt && Set.mem (UpdateElement.vars update_x_opt) x)
                 (* update: x' = T and T not in guard *)
                 || (not @@ Set.is_empty (UpdateElement.vars update_x_opt))
                    && Set.is_subset (UpdateElement.vars update_x_opt)
                         ~of_:(Set.diff (TransitionLabel.tmp_vars t) (Guard.vars (TransitionLabel.guard t))))
                 (* x not in costs *)
                 && (not @@ Set.mem (UpdateElement.vars @@ TransitionLabel.cost t) x)
                 (* x not in another update *)
                 && Set.for_all (TransitionLabel.input_vars t) ~f:(fun y ->
                        if Var.equal x y then
                          true
                        else
                          let update_y = TransitionLabel.update t y in
                          if Option.is_none update_y then
                            true
                          else
                            let update_y_opt = Option.value_exn update_y in
                            not @@ Set.mem (UpdateElement.vars update_y_opt) x)))
  in
  if not @@ Set.is_empty unnecessary_vars then (
    Logger.(
      log logger INFO (fun () ->
          ("TransformTmpVarsToNonDet", [ ("removed vars", VarSet.to_string unnecessary_vars) ])));
    MaybeChanged.changed @@ Program.remove_non_contributors unnecessary_vars program)
  else
    MaybeChanged.same program
