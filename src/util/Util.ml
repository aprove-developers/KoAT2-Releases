open Batteries

let get_all (xs: ('a Option.t) list): ('a list) Option.t =
  let combine result maybe =
    Option.bind maybe (fun x -> Option.map (fun list -> x :: list) result) in
  List.fold_left combine (Some []) xs 

(* Computes the maximum of the enum if non-empty, else returns None. *)
let max_option (greater: 'a -> 'a -> bool) (enum: 'a Enum.t): 'a Option.t =
  let f = function
    | (None, y) -> Some y
    | (Some x, y) -> if greater x y then Some x else Some y in
  Enum.fold (curry f) None enum

let intersection (p: 'a -> 'a -> bool) (enum1: 'a Enum.t) (enum2: 'a Enum.t) =
  Enum.cartesian_product enum1 enum2
  |> Enum.filter (uncurry p)
  |> Enum.map Tuple2.first

let without (p: 'a -> 'a -> bool) (toBeRemoved: 'a Enum.t) (enum: 'a Enum.t) =
  Enum.filter (fun v -> not (Enum.exists (p v) (Enum.clone toBeRemoved))) enum
