open Batteries

module Make(Value : PolyTypes.Ring) =
  struct
    module Valuation_ = Valuation.Make(Value)
    type valuation = Valuation.Make(Value).t
    module Map = Map.Make(Var)

    type t = int Map.t [@@deriving eq, ord]
           
    type value = Value.t

    let make list =
      let addEntry map (var, n) = Map.modify_def 0 var ((+) n) map in
      List.fold_left addEntry Map.empty list

    let lift var n = make [(var, n)]

    let fold ~const ~var ~times ~pow mon =
      Map.fold (fun key n a -> times (pow (var key) n) a) mon (const Value.one)
                   
    let vars mon =
      VarSet.of_enum (Map.keys mon)
             
    let degree mon =
      Map.fold (fun _ -> (+)) mon 0 

    let degree_variable var mon =
      Map.find_default 0 var mon

    let delete_var = Map.remove

    (* Probably inefficient but not important in to_string *)
    let to_string ?(to_file = false) mon =
      if Map.is_empty mon then
        "1"
      else
        let entry_string key n =
          Var.to_string ~to_file key ^ (if n != 1 then "^" ^ string_of_int n else "")
        and ((var, n), without_first) = Map.pop mon in
        Map.fold (fun key n str -> str ^ (entry_string key n)) without_first (entry_string var n)

    let is_univariate_linear mon =
      degree mon == 1
      
    let (=~=) = Map.equal (==) 

    let rename varmapping mon =
      let addRenamed var n map = Map.add (RenameMap.find var varmapping var) n map in
      Map.fold addRenamed mon Map.empty

    let mul =
      let addPowers ?(p1=0) ?(p2=0) _ = p1 + p2 in
      Map.merge (fun key p1 p2 -> Some (addPowers ?p1 ?p2 ()))  

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
