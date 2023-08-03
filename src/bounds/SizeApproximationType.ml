open Batteries
open ProgramTypes
open RVGTypes

module Make(B : BoundType.Bound)
           (RV: ProgramTypes.RV) =
  struct
    let logger = Logging.(get Approximation)

    module Map = Hashtbl.Make(RV.RVTuple_)

    type t = B.t Map.t

    let empty = Map.create

    let get map rv =
      let execute () =
        Map.find_option map rv
        |? B.infinity
      in Logger.with_log logger Logger.DEBUG
                         (fun () -> "sizebound", ["rv", RV.to_id_string rv])
                         ~result:B.to_string
                         execute
    let enum = Map.enum
    let of_enum = Map.of_enum

    let add ?(simplifyfunc=identity) bound rv map =
      (* We do not want to log trivial size bounds *)
      if not (B.is_infinity bound) then
        ( try
            let old_bound =  Map.find map rv in
            Map.modify rv (simplifyfunc % B.keep_simpler_bound bound) map;
            let new_bound =  Map.find map rv in
            Logger.log logger Logger.DEBUG
              (fun () -> "modified_size_bound", [ "rv", RV.to_id_string rv
                                             ; "bound", B.to_string bound
                                             ; "old_bound", B.to_string old_bound
                                             ; "new_bound", B.to_string new_bound])
          with
          | Not_found -> (
            Map.add map rv (simplifyfunc bound);
            Logger.log logger Logger.INFO
              (fun () -> "add_size_bound", ["rv", RV.to_id_string rv; "bound", B.to_string bound])
        ));
      map

    let add_all ?(simplifyfunc=identity) bound scc map =
      List.iter (fun rv -> ignore (add ~simplifyfunc:simplifyfunc bound rv map)) scc;
      map

    let to_formatted ?(pretty=false) size =
      Map.to_list size
      |> List.sort (fun (rv1,_) (rv2,_) -> RV.compare rv1 rv2)
      |> List.map
           (fun (rv, bound) -> FormattedString.mk_str_line @@
             "  " ^ RV.ids_to_string ~pretty rv ^ ": " ^ B.to_string ~pretty bound)
      |> FormattedString.mappend

    let to_string size =
      FormattedString.render_string @@ to_formatted size

  end

module EqMake(B: BoundType.Bound)
             (RV: ProgramTypes.RV)(RV': ProgramTypes.RV)
             (RVTupleEq: functor(F: functor(_: ProgramTypes.RVTuple) -> sig type t end) -> sig
                val proof: (F(RV.RVTuple_).t, F(RV'.RVTuple_).t) Util.TypeEq.t
              end) = struct

  let proof: (Make(B)(RV).t, Make(B)(RV').t) Util.TypeEq.t =
    let module T = RVTupleEq(functor (F: ProgramTypes.RVTuple) -> struct
        type t = B.t Hashtbl.Make(F).t (* get rid of polymorphism in Hashtbl.Make(_).t *)
      end)
    in
    T.proof

end
