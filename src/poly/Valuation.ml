open OurBase

module MakeOverIndeterminate (I : PolyTypes.Indeterminate) (Value : PolyTypes.Ring) = struct
  module M = MakeMapCreators1 (I)

  type value = Value.t
  type t = value M.t
  type indeterminate = I.t

  let from entries =
    let addEntry entry =
      match entry with
      | key, value -> Map.add_exn ~key ~data:value
    in
    List.fold_left ~f:(fun map keyadder -> keyadder map) ~init:M.empty (List.map ~f:addEntry entries)


  let zero vars = from (List.map ~f:(fun var -> (var, Value.zero)) vars)
  let eval indet t = Map.find_exn t indet
  let eval_opt indet t = Map.find t indet
  let is_defined valuation var = Map.mem valuation var
  let indeterminates t = List.map ~f:Tuple2.first (Map.to_alist t)

  let vars valuation =
    Sequence.map ~f:(Set.to_sequence % I.vars % Tuple2.first) (Map.to_sequence valuation)
    |> Sequence.join
    |> VarSet.stable_dedup_list % Sequence.to_list


  let bindings : t -> (indeterminate * value) Sequence.t = Map.to_sequence

  let to_string (valuation : t) =
    let output = Buffer.create 100 in
    Buffer.add_string output "{\n";
    Map.iteri
      ~f:(fun ~key ~data ->
        Buffer.add_string output @@ "  " ^ I.to_string key ^ ": " ^ Value.to_string data ^ ",\n")
      valuation;
    Buffer.add_string output "}\n";
    Buffer.contents output
end

module Make (Value : PolyTypes.Ring) = struct
  include MakeOverIndeterminate (VarIndeterminate) (Value)

  let from_native entries =
    from (List.map ~f:(fun (var, value) -> (Var.of_string var, Value.of_int value)) entries)
end
