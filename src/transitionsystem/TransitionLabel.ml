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
    id : int;
    update : Polynomial.t VarMap.t;
    guard : Guard.t;
    cost : Polynomial.t;
  }
  
let one = Polynomial.one

let make ?(cost=one) com_kind ~update ~guard =
  if com_kind <> "Com_1" then raise OnlyCom1Supported else
  {
    id = unique ();
    update; guard; cost;
  }

let same lbl1 lbl2 =
  lbl1.id = lbl2.id

let equivalent lbl1 lbl2 =
  VarMap.equal Polynomial.equal lbl1.update lbl2.update
  && Guard.equal lbl1.guard lbl2.guard
  && Polynomial.equal lbl1.cost lbl2.cost

let compare_same lbl1 lbl2 =
  Int.compare lbl1.id lbl2.id

let compare_equivalent lbl1 lbl2 =
  if VarMap.compare Polynomial.compare lbl1.update lbl2.update != 0 then
    VarMap.compare Polynomial.compare lbl1.update lbl2.update
  else if Guard.compare lbl1.guard lbl2.guard != 0 then
    Guard.compare lbl1.guard lbl2.guard
  else if Polynomial.compare lbl1.cost lbl2.cost != 0 then
    Polynomial.compare lbl1.cost lbl2.cost
  else
    0
let take_last n xs =
  xs
  |> List.rev
  |> List.take n
  |> List.rev
    
(* TODO Pattern <-> Assigment relation *)
let mk ?(cost=one) ~com_kind ~targets ~patterns ~guard ~vars =
  if List.length targets != 1 then raise RecursionNotSupported else
    if com_kind <> "Com_1" then raise OnlyCom1Supported else
      let (target, assignments) = List.hd targets in
      let assignments_with_trivial =
          assignments @ List.map Polynomial.of_var (take_last ((List.length patterns) - (List.length assignments)) patterns) in
      let appended_patterns =
          patterns @ Var.fresh_id_list Var.Int ((List.length assignments) - (List.length patterns)) in
      (* TODO Better error handling in case the sizes differ *)
      (List.enum appended_patterns, List.enum assignments_with_trivial)
      |> (uncurry Enum.combine)
      |> Enum.map (fun (var, assignment) -> VarMap.add var assignment)
      |> Enum.fold (fun map adder -> adder map) VarMap.empty 
      |> fun update -> { id = unique ();
                         update; guard; cost;}
                   
let append t1 t2 =
  let module VarTable = Hashtbl.Make(Var) in
  let nondet_vars = VarTable.create 3 in
  let substitution update_map var =
    VarMap.Exceptionless.find var update_map
    |? Polynomial.of_var
         (* Variables which are nondeterministic in the preceding transition are represented by fresh variables. *)
         (VarTable.find_option nondet_vars var
          |> Option.default_delayed (fun () ->
                 let nondet_var = Var.fresh_id Var.Int () in
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
    id = unique ();
    update = new_update;
    guard = new_guard;
    cost = Polynomial.(t1.cost + t2.cost);
  }

let id t = t.id
             
let update t var = VarMap.Exceptionless.find var t.update                    
                 
let guard t = t.guard

let map_guard f label =
  { label with guard = f label.guard }

let cost t = t.cost

let vars_ {update; guard; cost; _} =
  VarMap.fold (fun _ -> VarSet.union % Polynomial.vars) update VarSet.empty
  |> (VarSet.union % VarSet.of_enum % VarMap.keys) update
  |> (VarSet.union % Guard.vars) guard
  |> (VarSet.union % Polynomial.vars) cost

(* TODO May invalidate through invariant generation! *)
let vars = Util.memoize ~extractor:id vars_


let default = {
    id = 0;
    update = VarMap.empty;
    guard = Guard.mk_true;
    cost = one;
  }
            
let update_to_string update =
  if VarMap.is_empty update then
    "true"
  else
    update
    |> VarMap.bindings
    |> List.map (fun (var,poly) -> (Var.to_string var, Polynomial.to_string poly))
    |> List.split
    |> fun (xs,ys) -> "("^(String.concat "," xs)^") -> ("^(String.concat "," ys)^")"

let guard_to_string label =
  if 
    Guard.is_true label.guard then "" 
  else
    Guard.to_string label.guard

let to_string label =          
  let guard = if Guard.is_true label.guard then "" else "\n" ^ Guard.to_string ~comp:"&le;" ~conj:"&and;" label.guard in
  let cost = if Polynomial.is_one label.cost then "" else Polynomial.to_string label.cost ^ "&euro;" ^ ", " in
  "ID: " ^ string_of_int label.id ^ ", " ^ cost ^ update_to_string label.update ^ guard
  
let update_to_string_lhs t =
  let update = t.update in
    if VarMap.is_empty update then
      ""
    else
      update
      |> VarMap.bindings
      |> List.map (fun (var,poly) -> (Var.to_string var, Polynomial.to_string poly))
      |> List.split
      |> Tuple2.first
      |> fun xs -> "("^(String.concat "," xs)^")"

let update_to_string_rhs t =
  let update = t.update in
    if VarMap.is_empty update then
      ""
    else
      update
      |> VarMap.bindings
      |> List.map (fun (var,poly) -> (Var.to_string var, Polynomial.to_string poly))
      |> List.split
      |> Tuple2.second
      |> fun xs -> "("^(String.concat "," xs)^")"
    

let to_id_string =          
  string_of_int % id

let input_vars t = 
  t.update
  |> VarMap.keys
  |> VarSet.of_enum  

let input_size t =
  t
  |> input_vars
  |> VarSet.cardinal 

(** Whenever this function is invoked it is ensured that there are enough standard variables  *)
let standard_renaming standard_vars t =
  standard_vars
  |> List.take (input_size t)
  |> List.combine ((VarSet.elements % input_vars) t)
  |> RenameMap.from
  
let rename_update update rename_map =
  update
  |> VarMap.enum 
  |> Enum.map (fun (key, value) -> (RenameMap.find key rename_map ~default:key), Polynomial.rename rename_map value)
  |> VarMap.of_enum
  
  
let rename standard_vars t =
  let rename_map = standard_renaming standard_vars t in
  {
    id = t.id;
    update = rename_update t.update rename_map;
    guard = Guard.rename t.guard rename_map;
    cost = Polynomial.rename rename_map t.cost;
  }
       
