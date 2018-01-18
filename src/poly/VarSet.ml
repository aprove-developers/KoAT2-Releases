open Batteries

include Set.Make(Var)

let map_to_set f varset =
  varset
  |> to_list
  |> Set.of_list
  |> Set.map f
  
let map_to_list f varset =
  varset
  |> to_list
  |> List.map f

let map_to_array f varset =
  varset
  |> to_array
  |> Array.map f

(* There is no to_string for sets in batteries,
   but there is a very efficient print function which is however a bit inconvenient to use. *)
let to_string varset =
  let output = IO.output_string () in
  print (fun output var -> IO.nwrite output (Var.to_string var)) output varset;
  IO.close_out output
       
let of_string_list list =
  list
  |> List.map Var.of_string
  |> of_list

let powerset set =
  let combine (result: t Enum.t) (x: Var.t) = Enum.append result (Enum.map (fun ys -> add x ys) (Enum.clone result)) in
  Enum.fold combine (Enum.singleton empty) (enum set)


type outer_t = t
(** Internal memoization for powersets *)
module Cache =
  Hashtbl.Make(
      struct
        type t = outer_t
        let equal = equal
        let hash = Hashtbl.hash
      end
    )
   
let table =
  Cache.create 3
  
let memoize f =  
  let g x = 
    match Cache.find_option table x with
    | Some y -> y
    | None ->
       let y = f x in
       Cache.add table x y;
       y
  in g

let sorted_powerset set =
  let f set =
    set
    |> powerset
    |> List.of_enum
    |> List.sort (fun a b -> Int.compare (cardinal a) (cardinal b))
  in
  memoize f set

