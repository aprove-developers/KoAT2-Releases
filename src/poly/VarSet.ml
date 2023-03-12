open OurBase

include OurBase.MakeSetCreators0(Var)

let equal = Set.equal

let map_to_list f varset =
  Set.to_list varset
  |> List.map ~f

let map_to_array f varset =
  Set.to_array varset
  |> Array.map ~f

let to_string ?(pretty=false) varset =
  Set.to_sequence varset
  |> Util.sequence_to_string ~f:(Var.to_string ~pretty)

let of_string_list list =
  list
  |> List.map ~f:Var.of_string
  |> of_list

type outer_t = t
(** Internal memoization for combinations *)
module Cache =
  Batteries.Hashtbl.Make(
      struct
        type t = int * outer_t
        let equal (max1, set1) (max2, set2) =
          Int.equal max1 max2
          && Set.equal set1 set2
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

let max set =
  Set.max_elt set

let combinations count set =
  let rec f (count, set) =
    if count == 0 then
      [empty]
    else
      f (count - 1, set)
      |> Sequence.of_list
      |> Sequence.map ~f:(fun varset ->
             Set.to_sequence set
             |> Sequence.filter ~f:(fun var -> max varset |> Option.map ~f:(fun max -> Var.compare var max > 0) |? true)
             |> Sequence.map ~f:(Set.add varset)
           )
      |> Sequence.join
      |> Sequence.to_list
  in
  memoize f (count, set)
