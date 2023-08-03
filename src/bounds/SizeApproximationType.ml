open OurBase
open ProgramTypes
open RVGTypes

type ('rvtuple_,'bound) size_approximation_t = ('rvtuple_,'bound) Hashtbl.t

module Make(B : BoundType.Bound)
           (RV: ProgramTypes.RV) =
  struct
    let logger = Logging.(get Approximation)

    type t = (RV.RVTuple_.t,B.t) size_approximation_t

    let empty size = Hashtbl.create (module RV.RVTuple_) ~size

    let get map rv =
      let execute () =
        Hashtbl.find map rv |? B.infinity
      in Logger.with_log logger Logger.DEBUG
                         (fun () -> "sizebound", ["rv", RV.to_id_string rv])
                         ~result:B.to_string
                         execute
    let to_sequence = Hashtbl.to_sequence
    let of_sequence sequence = Hashtbl.of_alist_exn (module RV.RVTuple_) (Sequence.to_list sequence)

    let add ?(simplifyfunc=identity) bound rv map =
      if not (B.is_infinity bound) then (
        let update_or_add_entry = function
          | None ->
            Logger.log logger Logger.INFO
              (fun () -> "add_size_bound", ["rv", RV.to_id_string rv; "bound", B.to_string bound]);
            Some (simplifyfunc bound)
          | Some old_bound ->
            let new_bound = simplifyfunc (B.keep_simpler_bound old_bound bound) in
            Logger.log logger Logger.DEBUG
              (fun () -> "modified_size_bound", [ "rv", RV.to_id_string rv
                                             ; "bound", B.to_string bound
                                             ; "old_bound", B.to_string old_bound
                                             ; "new_bound", B.to_string new_bound]);
            Some new_bound
        in
        Hashtbl.change map rv ~f:update_or_add_entry
      );
      map

    let add_all ?(simplifyfunc=identity) bound scc map =
      List.iter ~f:(fun rv -> ignore (add ~simplifyfunc:simplifyfunc bound rv map)) scc;
      map

    let to_formatted ?(pretty=false) size =
      Hashtbl.to_alist size
      |> List.sort ~compare:(fun (rv1,_) (rv2,_) -> RV.compare rv1 rv2)
      |> List.map
           ~f:(fun (rv, bound) -> FormattedString.mk_str_line @@
                "  " ^ RV.ids_to_string ~pretty rv ^ ": " ^ B.to_string ~pretty bound)
      |> FormattedString.mappend

    let to_string size =
      FormattedString.render_string @@ to_formatted size

  end
