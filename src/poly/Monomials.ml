open Batteries

module MakeOverIndeterminate(I : PolyTypes.Indeterminate)(Value : PolyTypes.Ring) =
  struct
    module Valuation_ = Valuation.MakeOverIndeterminate(I)(Value)
    type valuation = Valuation_.t
    module Map = Map.Make(I)

    type t = int Map.t [@@deriving eq, ord]
    type indeterminate = I.t

    type value = Value.t

    (* A monomial is integral if the variables can only take integral values *)
    let is_integral = Enum.for_all identity % Enum.map I.is_integral % Map.keys

    let of_enum enum =
      let addEntry map (var, n) = Map.modify_def 0 var ((+) n) map in
      Enum.fold addEntry Map.empty enum

    let make = of_enum % List.enum

    let to_enum = Map.enum

    let lift var n = make [(var, n)]

    let of_indeterminate var = make [(var, 1)]
    let of_var = of_indeterminate % I.of_var

    let fold ~const ~indeterminate ~times ~pow mon =
      Map.fold (fun key n a -> times (pow (indeterminate key) n) a) mon (const Value.one)

    let degree mon =
      Map.fold (fun _ -> (+)) mon 0

    let degree_variable var mon =
      Map.find_default 0 var mon

    let delete_indeterminate = Map.remove

    let to_map = fun t -> t
    let of_map = fun t -> t

    (* Probably inefficient but not important in to_string *)
    let to_string ?(to_file = false) ?(pretty = false) mon =
      if Map.is_empty mon then
        "1"
      else
        let entry_string key n =
          if pretty then let str = I.to_string ~pretty key in
            if n != 1 then "(" ^ str ^ ")" ^ Util.natural_to_superscript n else str
          else (I.to_string ~to_file key ^ (if n != 1 then "^" ^ string_of_int n else ""))
        in
        Map.bindings mon
        |> List.map (uncurry entry_string)
        |> String.concat "*"

    let is_univariate_linear mon =
      degree mon == 1

    let (=~=) = Map.equal (==)

    let rename (m: RenameMap.t) (mon: t) =
      Map.enum mon
      |> Enum.map (Tuple2.map1 (I.rename m))
      |> Map.of_enum

    let indeterminates (t:t) =
      let module S = Set.Make(I) in
      S.enum @@ S.of_enum @@ Map.keys t

    let vars t =
      Map.keys t
      |> Enum.map I.vars
      |> Enum.fold VarSet.union VarSet.empty

    let mul =
      let addPowers ?(p1=0) ?(p2=0) _ = p1 + p2 in
      Map.merge (fun key p1 p2 -> Some (addPowers ?p1 ?p2 ()))

    let pow t e =
      Map.map (fun i -> i*e) t

    let eval_f mon f =
      Map.fold (fun var n result -> Value.mul result (f var)) mon Value.one

    (* Idea: Merge each var from the monomial with its value from the valuation, do the exponentation and fold the result with a multiplication *)
    let eval mon valuation =
      let power ?(n=0) v = (Value.pow v n)
      and valuation_map = Map.of_enum (Valuation_.bindings valuation) in
      let merge key n value = Some (power ?n (Option.get value)) in
      Map.merge merge mon valuation_map
      |> fun map -> Map.fold (fun key -> Value.mul) map Value.one

    let one = Map.empty
  end

module Make = MakeOverIndeterminate(VarIndeterminate)
