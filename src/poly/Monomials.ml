open OurBase

module MakeOverIndeterminate (I : PolyTypes.Indeterminate) (Value : PolyTypes.Ring) = struct
  module Valuation_ = Valuation.MakeOverIndeterminate (I) (Value)

  type valuation = Valuation_.t

  module M = MakeMapCreators1 (I)

  type t = int M.t (*  [@@deriving ord] *)

  let equal t1 t2 = Map.equal Int.equal t1 t2
  let compare t1 t2 = Map.compare_direct Int.compare t1 t2

  type indeterminate = I.t
  type value = Value.t

  (* A monomial is integral if the variables can only take integral values *)
  let is_integral = Map.for_alli ~f:(fun ~key ~data -> I.is_integral key)

  let of_sequence seq =
    let addEntry map (var, n) =
      Map.change map var ~f:(Option.some % Option.value_map ~f:(( + ) n) ~default:n)
    in
    Sequence.fold ~f:addEntry ~init:M.empty seq


  let make = of_sequence % Sequence.of_list
  let to_sequence m = Map.to_sequence m
  let lift var n = make [ (var, n) ]
  let of_indeterminate var = make [ (var, 1) ]
  let of_var = of_indeterminate % I.of_var

  let fold ~const ~indeterminate ~times ~pow mon =
    Map.fold ~f:(fun ~key ~data a -> times (pow (indeterminate key) data) a) mon ~init:(const Value.one)


  let degree mon = Map.fold ~f:(fun ~key ~data a -> data + a) mon ~init:0
  let degree_variable var mon = Option.value (Map.find mon var) ~default:0
  let delete_indeterminate indet map = Map.remove map indet

  (* Probably inefficient but not important in to_string *)
  let to_string ?(to_file = false) ?(pretty = false) mon =
    if Map.is_empty mon then
      "1"
    else
      let entry_string key n =
        if pretty then
          let str = I.to_string ~pretty key in
          if n != 1 then
            "(" ^ str ^ ")" ^ Util.natural_to_superscript n
          else
            str
        else
          I.to_string ~to_file key
          ^
          if n != 1 then
            "^" ^ string_of_int n
          else
            ""
      in
      Map.to_alist mon |> List.map ~f:(uncurry entry_string) |> String.concat ~sep:"*"


  let is_univariate_linear mon = degree mon == 1
  let ( =~= ) m1 m2 = Map.equal ( == ) m1 m2

  let rename (m : RenameMap.t) (mon : t) =
    Map.to_sequence mon |> Sequence.map ~f:(Tuple2.map1 (I.rename m)) |> M.of_sequence_exn


  let indeterminates (t : t) = Set.stable_dedup_list (module I) @@ Map.keys t
  let vars t = Map.keys t |> List.map ~f:I.vars |> List.fold ~f:Set.union ~init:VarSet.empty

  let mul =
    let addPowers ?(p1 = 0) ?(p2 = 0) _ = p1 + p2 in
    Map.merge ~f:(fun ~key me ->
        Some
          (let p1, p2 = Map.Merge_element.(left me, right me) in
           addPowers ?p1 ?p2 ()))


  let pow t e = Map.map ~f:(fun i -> i * e) t
  let eval_f mon f = Map.fold ~f:(fun ~key ~data result -> Value.mul result (f key)) mon ~init:Value.one

  (* Idea: Merge each var from the monomial with its value from the valuation, do the exponentation and fold the result with a multiplication *)
  let eval mon valuation =
    let power ?(n = 0) v = Value.pow v n
    and valuation_map = M.of_sequence_exn (Valuation_.bindings valuation) in
    let merge n value = Some (power ?n (Option.value_exn value)) in
    Map.merge ~f:Map.Merge_element.(fun ~key me -> merge (left me) (right me)) mon valuation_map |> fun map ->
    Map.fold ~f:(fun ~key ~data -> Value.mul data) map ~init:Value.one


  let one = M.empty
end

module Make = MakeOverIndeterminate (VarIndeterminate)
