open Batteries
open ProgramTypes
open RVGTypes

module Make(Num : PolyTypes.OurNumber)
           (RV: RVGTypes.RVType) =
  struct
    module B = BoundType.Make_BoundOver(Num)
    let logger = Logging.(get Approximation)

    module Map = Hashtbl.Make(struct include RV let equal = same end)

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
      |> List.sort (fun (rv1,_) (rv2,_) -> RV.compare_same rv1 rv2)
      |> List.map
           (fun (rv, bound) -> FormattedString.mk_str_line @@
             "  " ^ RV.ids_to_string ~pretty rv ^ ": " ^ B.to_string ~pretty bound)
      |> FormattedString.mappend

    let to_string size =
      FormattedString.render_string @@ to_formatted size

    (** Very slow equality, only for testing purposes *)
    let equivalent size1 size2 =
      let module Set =
        Set.Make(struct type t = RV.t * B.t
                        let compare (rv1,bound1) (rv2,bound2) =
                          if RV.compare_same rv1 rv2 != 0 then
                            RV.compare_same rv1 rv2
                          else if B.(bound1 < bound2) |? false then
                            -1
                          else if B.(bound1 > bound2) |? false then
                            1
                          else
                            0
                 end)
      in
      let to_set = Set.of_enum % Map.enum in
      Set.equal (to_set size1) (to_set size2)

  end
