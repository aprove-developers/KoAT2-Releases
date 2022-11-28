open Batteries

module MakeOverIndeterminate(I: PolyTypes.Indeterminate)(Value : PolyTypes.Ring) =
  struct
    module M = Map.Make(I)
    type value = Value.t
    type t = value M.t
    type indeterminate = I.t

    let from entries =
      let addEntry entry = match entry with
        | (key, value) -> M.add key value in
      List.fold_left (fun map keyadder -> keyadder map) M.empty (List.map addEntry entries)

    let zero vars = from (List.map (fun var -> (var, Value.zero)) vars)

    let eval = M.find

    let eval_opt var valuation =
      try Some (M.find var valuation) with
        Not_found -> None

    let is_defined valuation var =
      M.mem var valuation

    let indeterminates t = List.map Tuple2.first (M.bindings t)

    let vars valuation =
      Enum.map (VarSet.enum % I.vars % Tuple2.first) (M.enum valuation)
      |> Enum.flatten
      |> VarSet.to_list % VarSet.of_enum

    let bindings = M.enum

    let to_string valuation =
      let output = IO.output_string () in
      M.print (fun output key -> IO.nwrite output (I.to_string key)) (fun output value -> IO.nwrite output (Value.to_string value)) output valuation;
      IO.close_out output
  end

module Make(Value: PolyTypes.Ring) =
  struct
    include MakeOverIndeterminate(VarIndeterminate)(Value)

    let from_native entries =
      from (List.map (fun (var, value) -> (Var.of_string var, Value.of_int value)) entries)
  end
