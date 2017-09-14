open Batteries

module Make(Var : PolyTypes.ID)(Value : PolyTypes.Ring) =
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
                                      
    let vars valuation = List.map (fun (key, value) -> key) (M.bindings valuation)

    let bindings = M.enum
  end
