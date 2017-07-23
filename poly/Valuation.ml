open Batteries

module MakeValuation(Var : PolyTypes.ID)(Value : Number.Numeric) =
  struct
    module M = Map.Make(Var)
    type var = Var.t
    type value = Value.t
    type t = value M.t
    let from entries =
      let addEntry entry = match entry with
        | (key, value) -> M.add key value in
      List.fold_left (fun map keyadder -> keyadder map) M.empty (List.map addEntry entries)
    let zero vars = from (List.map (fun var -> (var, Value.zero)) vars)
    let eval = M.find
    let vars valuation = List.map (fun (key, value) -> key) (M.bindings valuation)
  end
