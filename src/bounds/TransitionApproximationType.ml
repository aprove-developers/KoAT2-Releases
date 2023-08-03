open OurBase

type ('trans,'bound) transition_approximation_t = string * ('trans, 'bound) Hashtbl.t

module type ApproximableTransition = sig
  type program
  type t

  val id: t -> int
  val to_id_string: t -> string
  val compare: t -> t -> int
  val all_from_program: program -> t Sequence.t
  val ids_to_string: ?pretty:bool -> t -> string

  val sexp_of_t: t -> Sexp.t
  val hash: t -> int
end

module MakeDefaultApproximableTransition(PM: ProgramTypes.ProgramModules) = struct
  type program = PM.Program.t
  let all_from_program =
    Set.to_sequence % PM.Program.transitions

  include PM.Transition
  let ids_to_string ?(pretty=false) =
    PM.TransitionLabel.ids_to_string ~pretty % PM.Transition.label
end

module Make(B : BoundType.Bound)
           (T: ApproximableTransition) =
  struct
    let logger = Logging.(get Approximation)

    (** TODO improve type safety by making a hash table over transitions *)
    type t = (T.t,B.t) transition_approximation_t

    let empty name size = (name, Hashtbl.create (module T) ~size)

    let get (name,map) t =
      let execute () = Hashtbl.find map t |? B.infinity in
      Logger.with_log logger Logger.DEBUG
                         (fun () -> name ^ "bound", ["transition", T.to_id_string t])
                         ~result:B.to_string
                         execute

    let sum appr program =
      Sequence.fold ~f:(fun result trans -> B.(get appr trans + result)) ~init:B.zero (T.all_from_program program)

    let add ?(simplifyfunc=identity) bound transition (name,map) =
      Hashtbl.change map transition
        ~f:(Option.some % Option.value_map ~f:(simplifyfunc % B.keep_simpler_bound bound) ~default:(simplifyfunc bound));
      Logger.log logger Logger.INFO
        (fun () -> "add_" ^ name ^ "_bound", ["transition", T.to_id_string transition; "bound", B.to_string bound]);
      (name, map)

    let all_bounded appr =
      Sequence.for_all ~f:(fun t -> not (B.equal (get appr t) B.infinity))

    let to_formatted ?(pretty=false) ?(termination_only=false) transitions t =
      transitions
      |> List.sort ~compare:T.compare
      |> List.map ~f:(fun trans -> trans, get t trans)
      |> List.map ~f:(fun (trans,b) -> FormattedString.mk_str_line @@ "  " ^ T.ids_to_string ~pretty trans ^ ": " ^ B.to_string ~pretty ~termination_only b)
      |> FormattedString.mappend

    let to_string ?(termination_only=false) transitions t =
      FormattedString.render_string @@ to_formatted ~termination_only transitions t
  end
