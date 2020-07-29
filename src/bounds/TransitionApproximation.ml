open Batteries
open ProgramTypes
   
let logger = Logging.(get Approximation) 
           
type t = string * (int, Bound.t) Hashtbl.t
       
let empty name size = (name, Hashtbl.create size)

let get_id (name,map) id =
  let execute () =
    Hashtbl.find_option map id |? Bound.infinity
  in Logger.with_log logger Logger.DEBUG
                     (fun () -> name ^ "bound", ["transition", string_of_int id])
                     ~result:Bound.to_string
                     execute
                    
let get (name,map) transition =
  get_id (name,map) (Transition.id transition)

let sum appr program =
  TransitionGraph.fold_edges_e (fun transition result -> Bound.(get appr transition + result)) (Program.graph program) Bound.zero

let sum_available (name,map) =
  Hashtbl.fold (fun transition bound result -> Bound.(bound + result)) map Bound.zero
  
let add bound transition (name,map) =
  (try
     Hashtbl.modify (Transition.id transition) (Bound.min bound) map
   with
   | Not_found -> Hashtbl.add map (Transition.id transition) bound);
  Logger.log logger Logger.INFO
    (fun () -> "add_" ^ name ^ "_bound", ["transition", Transition.to_id_string transition; "bound", Bound.to_string bound]);
  (name, map)

let all_bounded appr =
  List.for_all (fun t -> not (Bound.equal (get appr t) Bound.infinity))
  
let to_string ?(html=false) transitions (name,map) =
  let sep = if html then "<br>" else "\n" in
  let output = IO.output_string () in
  transitions
  |> TransitionSet.to_list
  |> List.sort Transition.compare_same
  |> List.map (fun t -> t, Hashtbl.find_option map (Transition.id t) |? Bound.infinity)
  |> List.print
       ~first:"  "
       ~last:sep
       ~sep:(sep ^ "  ")
       (fun output (t,b) -> IO.nwrite output (Transition.to_id_string t ^ ": " ^ Bound.to_string b))
       output;  
  IO.close_out output

(** Very slow equality, only for testing purposes *)
let equivalent (name1,map1) (name2,map2) =
  let module Set =
    Set.Make(struct type t = int * Bound.t
                    let compare (id1,bound1) (id2,bound2) =
                      if Int.compare id1 id2 != 0 then
                        Int.compare id1 id2
                      else if Bound.(bound1 < bound2) |? false then
                        -1
                      else if Bound.(bound1 > bound2) |? false then
                        1
                      else
                        0
             end)
  in
  let to_set = Set.of_enum % Hashtbl.enum in
  Set.equal (to_set map1) (to_set map2)
