open ID

module type Valuation =
  sig
    type t
    type var
    type value = Big_int.big_int
    val from : (var * value) list -> t
    val zero : var list -> t
    val eval : var -> t -> value
    val vars : t -> var list
  end

module type ValuationFunctor =
  functor (Var : ID) -> Valuation with type var = Var.t
                          
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

module StringValuation = MakeValuation(StringID)
