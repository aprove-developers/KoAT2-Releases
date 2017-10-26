open Batteries
open Polynomials
   
module Guard = Constraints.Constraint
type polynomial = Polynomial.t
module Map = Map.Make(Var)
           
exception RecursionNotSupported
exception OnlyCom1Supported
        
type kind = [ `Lower | `Upper ] [@@deriving eq, ord]

type t = {
    start : string;
    target : string;
    update : polynomial Map.t;
    guard : Guard.t;
    cost : polynomial;
    (* TODO Transitions should have costs *)
  }
  
let one = Polynomial.one

let make ?(cost=one) com_kind ~start ~target ~update ~guard =
  if com_kind <> "Com_1" then raise OnlyCom1Supported else
  {
    start; target; update; guard; cost;
  }
                                             
                                             
(* TODO Pattern <-> Assigment relation *)
let mk ?(cost=one) ~com_kind ~start ~targets ~patterns ~guard ~vars =
  if List.length targets != 1 then raise RecursionNotSupported else
    if com_kind <> "Com_1" then raise OnlyCom1Supported else
      let (target, assignments) = List.hd targets in
      (* TODO Better error handling in case the sizes differ *)
      (List.enum patterns, List.enum assignments)
      |> Enum.combine
      |> Enum.map (fun (var, assignment) -> Map.add var assignment)
      |> Enum.fold (fun map adder -> adder map) Map.empty 
      |> fun update -> { start; target; update; guard; cost;}
                   
let equal t1 t2 =
     t1.start = t2.start
  && t1.target = t2.target
  
let compare t1 t2 = 
  let cmp = String.compare t1.start t2.start in
  if cmp = 0 then
    String.compare t1.target t2.target
  else cmp
  
let start t = t.start
            
let target t = t.target
             
let update t var = Map.Exceptionless.find var t.update                    
                 
let guard t = t.guard

let cost t = t.cost
            
let default = {   
    start = "";
    target = "";
    update = Map.empty;
    guard = Guard.mk_true;
    cost = one;
  }
            
let update_to_string_list update =
  if Map.is_empty update then
    "true"
  else
    let entry_string var poly = Var.to_string var ^ "' := " ^ Polynomial.to_string poly
    and ((var, poly), without_first) = Map.pop update in
    Map.fold (fun var poly result -> result ^ " && " ^ entry_string var poly) without_first (entry_string var poly)

let to_string label =          
  let guard = if Guard.is_true label.guard then "" else " && " ^ Guard.to_string label.guard in
  update_to_string_list label.update ^ guard
