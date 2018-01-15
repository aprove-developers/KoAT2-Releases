open Batteries
open Program.Types
   
let logger = Logging.(get Approximation) 

module ApproximatedTransition =
  struct
    include Transition
    let equal = same
  end
module Map = Hashtbl.Make(ApproximatedTransition)
           
type t = string * Bound.t Map.t
       
let empty name size = (name, Map.create size)
          
let get (name,map) transition =
  let execute () =
    Map.find_option map transition |? Bound.infinity
  in Logger.with_log logger Logger.DEBUG
                     (fun () -> name ^ "bound", ["transition", Transition.to_id_string transition])
                     ~result:Bound.to_string
                     execute

let get_between (name,map) src target =
  map
  |> Map.filteri (fun (l,_,l') _ -> Location.(equal l src && equal l' target))
  |> Map.values
  |> Option.some
  |> Option.filter (fun values -> Enum.count values = 1)
  |> fun option -> Option.bind option Enum.get

let sum appr program =
  TransitionGraph.fold_edges_e (fun transition result -> Bound.(get appr transition + result)) (Program.graph program) Bound.zero

let sum_available (name,map) =
  Map.fold (fun transition bound result -> Bound.(bound + result)) map Bound.zero
  
let add bound transition (name,map) =
  let execute () =
    (try
       Map.modify transition (Bound.min bound) map
     with Not_found -> Map.add map transition bound);
    (name, map)
  in Logger.with_log logger Logger.DEBUG
                     (fun () -> "add_" ^ name ^ "bound", ["transition", Transition.to_id_string transition; "bound", Bound.to_string bound])
                     execute

let all_bounded appr =
  List.for_all (fun t -> not (Bound.equal (get appr t) Bound.infinity))
  
let to_string (name,map) =
  let output = IO.output_string () in
  map
  |> Map.to_list
  |> List.sort (fun (t1,b1) (t2,b2) -> Transition.compare_same t1 t2)
  |> List.print
       ~first:"  "
       ~last:"\n"
       ~sep:"\n  "
       (fun output (t,b) -> IO.nwrite output (Transition.to_id_string t ^ ": " ^ Bound.to_string b))
       output;  
  IO.close_out output

(** Very slow equality, only for testing purposes *)
let equivalent (name1,map1) (name2,map2) =
  let module Set =
    Set.Make(struct type t = Transition.t * Bound.t
                    let compare (t1,bound1) (t2,bound2) =
                      if Transition.compare_same t1 t2 != 0 then
                        Transition.compare_same t1 t2
                      else if Bound.(bound1 < bound2) |? false then
                        -1
                      else if Bound.(bound1 > bound2) |? false then
                        1
                      else
                        0
             end)
  in
  let to_set = Set.of_enum % Map.enum in
  Set.equal (to_set map1) (to_set map2)
