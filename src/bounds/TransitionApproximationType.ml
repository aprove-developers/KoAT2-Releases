open Batteries
open ProgramModules

module Make(Num : PolyTypes.OurNumber)
           (PM: ProgramTypes.ProgramModules) =
  struct
    open PM

    module B = BoundType.Make_BoundOver(Num)
    let logger = Logging.(get Approximation)

    type t = string * (int, B.t) Hashtbl.t

    let empty name size = (name, Hashtbl.create size)

    let get_id (name,map) id =
      let execute () =
        Hashtbl.find_option map id |? B.infinity
      in Logger.with_log logger Logger.DEBUG
                         (fun () -> name ^ "bound", ["transition", string_of_int id])
                         ~result:B.to_string
                         execute

    let get (name,map) transition =
      get_id (name,map) (Transition.id transition)

    let sum appr program =
      TransitionSet.fold (fun transition result -> B.(get appr transition + result)) (Program.transitions program) B.zero

    let add ?(simplifyfunc=identity) bound transition (name,map) =
      (try
         Hashtbl.modify (Transition.id transition) (simplifyfunc % B.keep_simpler_bound bound) map
       with
       | Not_found -> Hashtbl.add map (Transition.id transition) (simplifyfunc bound));
      Logger.log logger Logger.INFO
        (fun () -> "add_" ^ name ^ "_bound", ["transition", Transition.to_id_string transition; "bound", B.to_string bound]);
      (name, map)

    let all_bounded appr =
      List.for_all (fun t -> not (B.equal (get appr t) B.infinity))

    let to_formatted ?(pretty=false) transitions (name, map) =
      transitions
      |> List.sort Transition.compare_same
      |> List.map (fun t -> t, Hashtbl.find_option map (Transition.id t) |? B.infinity)
      |> List.map (fun (t,b) -> if pretty then
          FormattedString.mk_str_line @@ "  t" ^ (Transition.id t |> Util.natural_to_subscript) ^ ": " ^ B.to_string ~pretty:true b
        else
          FormattedString.mk_str_line @@ "  t" ^ (Transition.id t |> string_of_int) ^ ": " ^ B.to_string b)
      |> FormattedString.mappend

    let to_string transitions (name, map) =
      FormattedString.render_string @@ to_formatted transitions (name, map)

    (** Very slow equality, only for testing purposes *)
    let equivalent (name1,map1) (name2,map2) =
      let module Set =
        Set.Make(struct type t = int * B.t
                        let compare (id1,bound1) (id2,bound2) =
                          if Int.compare id1 id2 != 0 then
                            Int.compare id1 id2
                          else if B.(bound1 < bound2) |? false then
                            -1
                          else if B.(bound1 > bound2) |? false then
                            1
                          else
                            0
                 end)
      in
      let to_set = Set.of_enum % Hashtbl.enum in
      Set.equal (to_set map1) (to_set map2)
  end
