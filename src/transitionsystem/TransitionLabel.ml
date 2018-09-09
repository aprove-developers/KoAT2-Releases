open Batteries
open Polynomials
open Formulas
   
module Guard = Constraints.Constraint
type polynomial = Polynomial.t
module VarMap = Map.Make(Var)
           
exception RecursionNotSupported
exception OnlyCom1Supported
exception ProbabilitiesNotBetweenZeroAndOne
exception DifferentUpdatesAndProbabilities

type kind = [ `Lower | `Upper ] [@@deriving eq, ord]

module UpdateElement = 
  struct
    type t = Poly of Polynomial.t | Dist of ProbDistribution.t [@@deriving eq,ord]
    
    let to_string u = 
      match u with
        | Poly p -> "(Polynomial: " ^ (Polynomial.to_string p)  ^ ")"
        | Dist d -> "(Distribution: " ^ (ProbDistribution.to_string d) ^ ")" 

    let rename rename_map u = 
      match u with 
        | Poly p -> Poly (Polynomial.rename rename_map p)
        | Dist d -> Dist (ProbDistribution.rename rename_map d)

    let vars u = 
      match u with 
        | Poly p -> Polynomial.vars p
        | Dist d -> ProbDistribution.vars d

    let is_polynomial u = 
      match u with
        | Poly p -> true
        | otherwise -> false

  let substitute sub u = 
    match u with
      | Poly p -> Poly (Polynomial.substitute_f sub p)
      | Dist d -> Dist (ProbDistribution.substitute sub d)
  end
          
type t = {
    id : int;
    update : UpdateElement.t VarMap.t;
    guard : Guard.t;
    cost : Polynomial.t;
    probability : Float.t;
  }

let triples (list1) (list2) (list3) = List.map (fun ((x,y),z) -> (x,y,z)) (List.combine (List.combine list1 list2) list3)

let quatruples (list1) (list2) (list3) (list4) = List.map (fun ((x,y),(z,w)) -> (x,y,z,w)) (List.combine (List.combine list1 list2) (List.combine list3 list4))
  
let one = Polynomial.one

(* Generates a nonprobabilistic label and sets the probability to one *)
let make ?(cost = one)  com_kind ~update ~guard = 
  if com_kind <> "Com_1" then raise OnlyCom1Supported else
  {
    id = unique ();
    update; guard; cost; probability=1.;
  }

(* Generates a probabilistic label, needs a name to distinguish different labels belonging to the same transition *)
let make_prob ?(cost = one)  com_kind ~update ~guard ~id ~probability = 
  if com_kind <> "Com_1" then raise OnlyCom1Supported else
    if (probability > 1. || probability < 0.) then raise ProbabilitiesNotBetweenZeroAndOne else
  {
    id = id;
    update; guard; cost; probability=probability;
  }

let same lbl1 lbl2 =
  lbl1.id = lbl2.id

let equivalent lbl1 lbl2 =
  VarMap.equal UpdateElement.equal lbl1.update lbl2.update
  && Guard.equal lbl1.guard lbl2.guard
  && Polynomial.equal lbl1.cost lbl2.cost
  && Float.equal lbl1.probability lbl2.probability

let compare_same lbl1 lbl2 =
  Int.compare lbl1.id lbl2.id

let compare_equivalent lbl1 lbl2 =
  if VarMap.compare UpdateElement.compare lbl1.update lbl2.update != 0 then
    VarMap.compare UpdateElement.compare lbl1.update lbl2.update
  else if Guard.compare lbl1.guard lbl2.guard != 0 then
    Guard.compare lbl1.guard lbl2.guard
  else if Polynomial.compare lbl1.cost lbl2.cost != 0 then
    Polynomial.compare lbl1.cost lbl2.cost
  else if Float.compare lbl1.probability lbl2.probability != 0 then
    Float.compare lbl1.probability lbl2.probability
  else
    0
let take_last n xs =
  xs
  |> List.rev
  |> List.take n
  |> List.rev
    
(* TODO Pattern <-> Assigment relation *)
let mk ?(cost = one) ~com_kind ~targets ~patterns ~guard ~vars =
  if List.length targets != 1 then raise RecursionNotSupported else
    if com_kind <> "Com_1" then raise OnlyCom1Supported else
      let (target, assignments) = List.hd targets in
      let assignments_with_trivial =
          assignments @ List.map (fun v -> UpdateElement.Poly (Polynomial.of_var v)) (take_last ((List.length patterns) - (List.length assignments)) patterns) in
      let appended_patterns =
          patterns @ Var.fresh_id_list Var.Int ((List.length assignments) - (List.length patterns)) in
      (* TODO Better error handling in case the sizes differ *)
      (List.enum appended_patterns, List.enum assignments_with_trivial)
      |> Enum.combine
      |> Enum.map (fun (var, assignment) -> VarMap.add var (assignment))
      |> Enum.fold (fun map adder -> adder map) VarMap.empty 
      |> fun update -> { id = unique ();
                        update; guard; cost; probability=1.;}
                        
let mk_prob ?(cost = one) ~com_kind ~targets ~patterns ~guard ~vars ~id ~probability =
  if List.length targets != 1 then raise RecursionNotSupported else
    if com_kind <> "Com_1" then raise OnlyCom1Supported else
      if (probability > 1. || probability < 0.) then raise ProbabilitiesNotBetweenZeroAndOne else
        let (target, assignments) = List.hd targets in
        (* TODO Better error handling in case the sizes differ *)
        (List.enum patterns, List.enum assignments)
        |> Enum.combine
        |> Enum.map (fun (var, assignment) -> VarMap.add var assignment)
        |> Enum.fold (fun map adder -> adder map) VarMap.empty 
        |> fun update -> { id = id;
                          update; guard; cost; probability=probability;}
                    
(*
Chaining can not be represented in the probabilistic update case 
*)
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
  let get_update_polynomials u_map = 
    VarMap.map (function u -> match u with 
                                | UpdateElement.Poly p -> p
                                | otherwise -> failwith "This case should be impossible"
               ) u_map
  in
  let new_update =
    VarMap.map (UpdateElement.substitute (substitution (get_update_polynomials t1.update))) t2.update
  and new_guard =
    Guard.Infix.(t1.guard && Guard.map_polynomial (Polynomial.substitute_f (substitution (get_update_polynomials t1.update))) t2.guard)
  in
  {
    id = unique ();
    update = new_update;
    guard = new_guard;
    cost = Polynomial.(t1.cost + t2.cost);
    probability = Float.(t1.probability * t2.probability);
  }

let append_guard t1 t2 =
  let module VarTable = Hashtbl.Make(Var) in
  let nondet_vars = VarTable.create 3 in
  let dist_vars = VarTable.create 3 in
  let handle_update var u = 
    match u with
      | UpdateElement.Poly p -> p
      | UpdateElement.Dist d -> Polynomial.of_var
         (VarTable.find_option dist_vars var
          |> Option.map fst
          |> Option.default_delayed (fun () ->
                 let nondet_var = Var.fresh_id Var.Int () in
                 VarTable.add dist_vars var (nondet_var,d);
                 nondet_var
               ) )
  in
  let substitution (update_map : UpdateElement.t VarMap.t) var =
    (VarMap.Exceptionless.find var update_map
     |> Option.map (handle_update var) )
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
  let dist_guard =
    VarTable.fold (fun _ (var,d) g -> Guard.Infix.(g && (ProbDistribution.guard d var)) ) dist_vars Guard.mk_true
  in
  let new_guard =
    Guard.Infix.(t1.guard && dist_guard && Guard.map_polynomial (Polynomial.substitute_f (substitution t1.update)) t2.guard)
  in
  new_guard

let id t = t.id
             
let update t var = VarMap.Exceptionless.find var t.update                    
let update_map t = t.update
                 
let guard t = t.guard

let map_guard f label =
  { label with guard = f label.guard }

let cost t = t.cost

let probability t = t.probability

let vars_ {update; guard; cost; _} =
  let update_element_vars u = 
    match u with 
      | UpdateElement.Poly p -> Polynomial.vars p
      | UpdateElement.Dist d -> ProbDistribution.vars d
  in
  VarMap.fold (fun _ -> VarSet.union % update_element_vars) update VarSet.empty
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
    probability = 1.
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


  
let update_to_string_lhs t =
  let update = t.update in
    if VarMap.is_empty update then
      ""
    else
      update
      |> VarMap.bindings
      |> List.map (fun (var,poly) -> (Var.to_string var, UpdateElement.to_string poly))
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
      |> List.map (fun (var,poly) -> (Var.to_string var, UpdateElement.to_string poly))
      |> List.split
      |> Tuple2.second
      |> fun xs -> "("^(String.concat "," xs)^")"
      
 let to_string label =          
  let guard = if Guard.is_true label.guard then "" else ":|:" ^ Guard.to_string label.guard in
  let cost = if Polynomial.is_one label.cost then "->" else "-{"^ Polynomial.to_string label.cost ^ "}>" ^ ", " in
  let probability = if (label.probability = 1.) then "" else "\n p:" ^ Float.to_string label.probability in
  "ID: " ^ string_of_int label.id ^ ", " ^ (update_to_string_lhs label)^ probability ^ cost ^ update_to_string_rhs label ^ guard 
  

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
  |> Enum.map (fun (key, value) -> (RenameMap.find key rename_map ~default:key), UpdateElement.rename rename_map value)
  |> VarMap.of_enum
  
  
let rename standard_vars t =
  let rename_map = standard_renaming standard_vars t in
  {
    id = t.id;
    update = rename_update t.update rename_map;
    guard = Guard.rename t.guard rename_map;
    cost = Polynomial.rename rename_map t.cost;
    probability = t.probability;
  }
       
