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
  let combine (result: t Enum.t) (x: Var.t) =
    result
    |> Enum.clone
    |> Enum.map (fun ys -> add x ys)
    |> Enum.append result 
  in
  set
  |> enum
  |> Enum.fold combine (Enum.singleton empty)

type outer_t = t
(** Internal memoization for combinations *)
module Cache =
  Hashtbl.Make(
      struct
        type t = int * outer_t
        let equal (max1, set1) (max2, set2) =
          Int.equal max1 max2
          && equal set1 set2
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

let combinations max set =
  let combine (result: t Enum.t) (x: Var.t) =
    result
    |> Enum.clone
    |> Enum.filter (fun ys -> cardinal ys <= max)
    |> Enum.map (add x)
    |> Enum.append result
  in
  set
  |> enum
  |> Enum.fold combine (Enum.singleton empty)

let max set =
  try
    Some (max_elt set)
  with Not_found -> None
  
let comb count set =
  let rec f (count, set) =  
    if count == 0 then
      List.singleton empty
    else
      f (count - 1, set)
      |> List.enum
      |> Enum.map (fun varset ->
             set
             |> enum
             |> Enum.filter (fun var -> varset |> max |> Option.map (fun max -> Var.compare var max > 0) |? true)
             |> Enum.map (fun var -> add var varset)
           )
      |> Enum.flatten
      |> List.of_enum
  in
  memoize f (count, set)
  
let sorted_combinations max set =
  Enum.seq 0 ((+) 1) ((>) (max+1))
  |> Enum.map (fun c -> comb c set)
  |> Enum.map List.enum
  |> Enum.flatten
  
