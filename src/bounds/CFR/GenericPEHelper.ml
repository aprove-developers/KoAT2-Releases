open! OurBase

module Make (A : PETypes.PEAdapter) = struct
  let overapprox_update update =
    Map.fold
      ~f:(fun ~key:var ~data:ue (new_update, guards) ->
        let ue_approx, guard = A.overapprox_indeterminates ue in
        (Map.add_exn ~key:var ~data:ue_approx new_update, Guard.mk_and guards guard))
      update
      ~init:(Map.empty (module Var), Guard.mk_true)
end
