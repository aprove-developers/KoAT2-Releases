open Batteries
open PolyTypes

module MakeValuation(Var : ID) =
  struct
    module M = Map.Make(Var)
    type var = Var.t
    type value = Big_int.big_int
    type t = value M.t
    let from entries =
      let addEntry entry = match entry with
        | (key, value) -> M.add key value in
      List.fold_left (fun map keyadder -> keyadder map) M.empty (List.map addEntry entries)
    let zero vars = from (List.map (fun var -> (var, Big_int.zero_big_int)) vars)
    let eval = M.find
    let vars valuation = List.map (fun (key, value) -> key) (M.bindings valuation)
  end

module StringValuation = MakeValuation(ID.StringID)
