open Batteries
open Polynomials
   
module Guard = Constraints.Constraint
type polynomial = Polynomial.t
module Map = Map.Make(Var)
           
exception RecursionNotSupported
        
type kind = Lower | Upper [@@deriving eq, ord]

type t = {
    name : string;
    start : string;
    target : string;
    update : polynomial Map.t;
    guard : Guard.t;
    (* TODO Transitions should have costs *)
  }

let make ~name ~start ~target ~update ~guard = {
    name; start; target; update; guard
  }
                                             
                                             
(* TODO Pattern <-> Assigment relation *)
let mk ~name ~start ~targets ~patterns ~guard ~vars =
  if List.length targets != 1 then raise RecursionNotSupported else
    let (target, assignments) = List.hd targets in
    List.combine patterns assignments
    |> List.map (fun (var, assignment) -> Map.add var assignment)
    |> List.fold_left (fun map adder -> adder map) Map.empty 
    |> fun update -> { name; start; target; update; guard }
                   
let equal t1 t2 =
  t1.name == t2.name
  
let compare t1 t2 = 
  if (t1 == t2) then 0
  else if (t1.name < t1.name) then (-1)
  else 1
  
let start t = t.start
            
let target t = t.target
             
let update t var = Map.Exceptionless.find var t.update                    
                 
let guard t = t.guard
            
let default = {   
    name = "default";
    start = "";
    target = "";
    update = Map.empty;
    guard = Guard.mk_true;
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
