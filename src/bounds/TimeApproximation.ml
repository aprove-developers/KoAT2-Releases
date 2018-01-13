open Batteries
open Program.Types
   
let logger = Logging.(get Approximation) 

module Time_Transition =
  struct
    include Transition
    let equal = same
  end
module Map = Hashtbl.Make(Time_Transition)
           
type t = Bound.t Map.t
       
let empty = Map.create
          
let get map transition =
  let execute () =
    Map.find_option map transition |? Bound.infinity
  in Logger.with_log logger Logger.DEBUG
                     (fun () -> "timebound", ["transition", Transition.to_id_string transition])
                     ~result:Bound.to_string
                     execute

let get_between map src target =
  map
  |> Map.filteri (fun (l,_,l') _ -> Location.(equal l src && equal l' target))
  |> Map.values
  |> Option.some
  |> Option.filter (fun values -> Enum.count values = 1)
  |> fun option -> Option.bind option Enum.get

let sum map program =
  TransitionGraph.fold_edges_e (fun transition result -> Bound.(get map transition + result)) (Program.graph program) Bound.zero

let sum_available map =
  Map.fold (fun transition bound result -> Bound.(bound + result)) map Bound.zero
  
let add bound transition map =
  let execute () =
    (try
       Map.modify transition (Bound.min bound) map
     with Not_found -> Map.add map transition bound);
    map
  in Logger.with_log logger Logger.DEBUG
                     (fun () -> "add_timebound", ["transition", Transition.to_id_string transition; "bound", Bound.to_string bound])
                     execute

let all_bounded map =
  List.for_all (fun t -> not (Bound.equal (get map t) Bound.infinity))
  
let to_string time =
  let output = IO.output_string () in
  time
  |> Map.to_list
  |> List.sort (fun (t1,b1) (t2,b2) -> Transition.compare_same t1 t2)
  |> List.print
       ~first:("  ")
       ~last:"\n"
       ~sep:"\n  "
       (fun output (t,b) -> IO.nwrite output (Transition.to_id_string t ^ ": " ^ Bound.to_string b))
       output;  
  IO.close_out output

(** Very slow equality, only for testing purposes *)
let equivalent time1 time2 =
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
  let to_set time = time |> Map.enum |> Set.of_enum in
  Set.equal (to_set time1) (to_set time2)
