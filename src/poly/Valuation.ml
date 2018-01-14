open Batteries

module Make(Value : PolyTypes.Ring) =
  struct
    module M = Map.Make(Var)
    type var = Var.t
    type value = Value.t
    type t = value M.t

    let from entries =
      let addEntry entry = match entry with
        | (key, value) -> M.add key value in
      List.fold_left (fun map keyadder -> keyadder map) M.empty (List.map addEntry entries)

    let from_native entries =
      from (List.map (fun (var, value) -> (Var.of_string var, Value.of_int value)) entries)

    let zero vars = from (List.map (fun var -> (var, Value.zero)) vars)

    let eval = M.find
             
    let eval_opt var valuation =
      try Some (M.find var valuation) with
        Not_found -> None
                                      
    let is_defined valuation var =
      M.mem var valuation

    let vars valuation = List.map (fun (key, value) -> key) (M.bindings valuation)

    let bindings = M.enum
    
    let to_string valuation =
      let output = IO.output_string () in
      M.print (fun output key -> IO.nwrite output (Var.to_string key)) (fun output value -> IO.nwrite output (Value.to_string value)) output valuation;
      IO.close_out output
  end
