open OurBase
open ProgramTypes
open RVGTypes

type ('rvtuple_, 'bound, 'rvtuple__cmp_wit) size_approximation_t =
  ('rvtuple_, 'bound, 'rvtuple__cmp_wit) Map.t

module Make (B : BoundType.Bound) (RV : ProgramTypes.RV) = struct
  let logger = Logging.(get Approximation)

  type t = (RV.t, B.t, RV.comparator_witness) size_approximation_t

  let empty = Map.empty (module RV)

  let get map rv =
    let execute () = Map.find map rv |? B.infinity in
    Logger.with_log logger Logger.DEBUG
      (fun () -> ("sizebound", [ ("rv", RV.to_id_string rv) ]))
      ~result:B.to_string execute


  let to_sequence m = Map.to_sequence m
  let of_sequence seq = Map.of_sequence_exn (module RV) seq

  let add ?(simplifyfunc = identity) bound rv map =
    if not (B.is_infinity bound) then
      let update_or_add_entry = function
        | None ->
            Logger.log logger Logger.INFO (fun () ->
                ("add_size_bound", [ ("rv", RV.to_id_string rv); ("bound", B.to_string bound) ]));
            Some (simplifyfunc bound)
        | Some old_bound ->
            let new_bound = simplifyfunc (B.keep_simpler_bound old_bound bound) in
            Logger.log logger Logger.DEBUG (fun () ->
                ( "modified_size_bound",
                  [
                    ("rv", RV.to_id_string rv);
                    ("bound", B.to_string bound);
                    ("old_bound", B.to_string old_bound);
                    ("new_bound", B.to_string new_bound);
                  ] ));
            Some new_bound
      in
      Map.change map rv ~f:update_or_add_entry
    else
      map


  let add_all ?(simplifyfunc = identity) bound scc map =
    List.fold ~f:(fun map rv -> add ~simplifyfunc bound rv map) scc ~init:map


  let to_formatted ?(pretty = false) size =
    Map.to_alist size
    |> List.sort ~compare:(fun (rv1, _) (rv2, _) -> RV.compare rv1 rv2)
    |> List.map ~f:(fun (rv, bound) ->
           FormattedString.mk_str_line @@ "  " ^ RV.ids_to_string ~pretty rv ^ ": "
           ^ B.to_string ~pretty bound)
    |> FormattedString.mappend


  let to_string size = FormattedString.render_string @@ to_formatted size
end
