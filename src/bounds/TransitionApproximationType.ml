open Batteries

module type ApproximableTransition = sig
  type program
  type t

  val id: t -> int
  val to_id_string: t -> string
  val compare_same: t -> t -> int
  val all_from_program: program -> t Enum.t
  val ids_to_string: ?pretty:bool -> t -> string
end

module MakeDefaultApproximableTransition(PM: ProgramTypes.ProgramModules) = struct
  type program = PM.Program.t
  let all_from_program = PM.TransitionSet.enum % PM.Program.transitions

  include PM.Transition
  let ids_to_string ?(pretty=false) =
    PM.TransitionLabel.ids_to_string ~pretty % PM.Transition.label
end

module Make(B : BoundType.Bound)
           (T: ApproximableTransition) =
  struct
    let logger = Logging.(get Approximation)

    (** TODO improve type safety by making a hash table over transitions *)
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
      get_id (name,map) (T.id transition)

    let sum appr program =
      Enum.fold (fun result trans -> B.(get appr trans + result)) B.zero (T.all_from_program program)

    let add ?(simplifyfunc=identity) bound transition (name,map) =
      (try
         Hashtbl.modify (T.id transition) (simplifyfunc % B.keep_simpler_bound bound) map
       with
       | Not_found -> Hashtbl.add map (T.id transition) (simplifyfunc bound));
      Logger.log logger Logger.INFO
        (fun () -> "add_" ^ name ^ "_bound", ["transition", T.to_id_string transition; "bound", B.to_string bound]);
      (name, map)

    let all_bounded appr =
      Enum.for_all (fun t -> not (B.equal (get appr t) B.infinity))

    let to_formatted ?(pretty=false) transitions (name, map) =
      transitions
      |> List.sort T.compare_same
      |> List.map (fun t -> t, Hashtbl.find_option map (T.id t) |? B.infinity)
      |> List.map (fun (t,b) -> if pretty then
          FormattedString.mk_str_line @@ "  " ^ T.ids_to_string ~pretty t ^ ": " ^ B.to_string ~pretty:true b
        else
          FormattedString.mk_str_line @@ "  " ^ T.ids_to_string ~pretty t ^ ": " ^ B.to_string b)
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

module EqMake(B: BoundType.Bound)
             (T: ApproximableTransition)(T': ApproximableTransition) = struct
  let proof: (Make(B)(T).t, Make(B)(T').t) Util.TypeEq.t = Util.TypeEq.Refl
end
