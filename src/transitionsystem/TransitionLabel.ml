open Batteries
open Polynomials
open Formulas
   
module Guard = Constraints.Constraint
type polynomial = Polynomial.t
module VarMap = Map.Make(Var)
           
exception RecursionNotSupported
exception OnlyCom1Supported
        
type kind = [ `Lower | `Upper ] [@@deriving eq, ord]

type t = {
    start : string;
    target : string;
    update : Polynomial.t VarMap.t;
    guard : Guard.t;
    cost : Polynomial.t;
  } [@@deriving eq, ord]
  
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
      |> Enum.map (fun (var, assignment) -> VarMap.add var assignment)
      |> Enum.fold (fun map adder -> adder map) VarMap.empty 
      |> fun update -> { start; target; update; guard; cost;}
                   
let append t1 t2 =
  let module VarTable = Hashtbl.Make(Var) in
  let nondet_vars = VarTable.create 3 in
  let substitution update_map var =
    VarMap.Exceptionless.find var update_map
    |? Polynomial.of_var
         (* Variables which are nondeterministic in the preceding transition are represented by fresh variables. *)
         (VarTable.find_option nondet_vars var
          |? (
            let nondet_var = Var.fresh_id () in
            VarTable.add nondet_vars var nondet_var;
            nondet_var
          )
         )
  in 
  let new_update =
    VarMap.map (Polynomial.substitute_f (substitution t1.update)) t2.update
  and new_guard =
    Guard.Infix.(t1.guard && Guard.map_polynomial (Polynomial.substitute_f (substitution t1.update)) t2.guard)
  in
  {
    start = t1.start;
    target = t2.target;
    update = new_update;
    guard = new_guard;
    cost = Polynomial.(t1.cost + t2.cost);
  }

let start t = t.start
            
let target t = t.target
             
let update t var = VarMap.Exceptionless.find var t.update                    
                 
let guard t = t.guard

let cost t = t.cost
            
let default = {   
    start = "";
    target = "";
    update = VarMap.empty;
    guard = Guard.mk_true;
    cost = one;
  }
            
let update_to_string_list update =
  if VarMap.is_empty update then
    "true"
  else
    let entry_string var poly = Var.to_string var ^ "' := " ^ Polynomial.to_string poly
    and ((var, poly), without_first) = VarMap.pop update in
    VarMap.fold (fun var poly result -> result ^ " && " ^ entry_string var poly) without_first (entry_string var poly)

let to_string label =          
  let guard = if Guard.is_true label.guard then "" else " && " ^ Guard.to_string label.guard in
  "Cost: " ^ Polynomial.to_string label.cost ^ ", " ^ update_to_string_list label.update ^ " && " ^ guard
