include Base

module Logger = Batteries.Logger

(*  TODO Comine with Util? *)

let (%) = Fn.compose

let flip = Fn.flip

let uncurry f = fun (a,b) -> f a b

let (|?) opt def = match opt with
  | Some r -> r
  | None   -> def

let tap (f: 'a -> unit) (value: 'a) = f value; value

let const x = fun _ -> x

module Tuple2 = Batteries.Tuple2
module Tuple3 = Batteries.Tuple3
module Tuple4 = Batteries.Tuple4
module Tuple5 = Batteries.Tuple5

module Set = struct
  include Set

  let powerset (type a cmp) (module M: Comparator.S with type t = a and type comparator_witness = cmp) (set: (a,cmp) t) =
    let combine result x =
      Sequence.append result (Sequence.map ~f:(fun ys -> Base.Set.add ys x) result)
    in
    Base.Sequence.fold ~f:combine ~init:(Sequence.singleton (empty (module M))) (Base.Set.to_sequence set)
end

module type Creators'0 = sig
  type elt
  type elt_comparator_witness
  include Set.Creators0
    with type elt := elt
     and type comparator_witness := elt_comparator_witness
     and type ('a, 'cmp) set = ('a, 'cmp) Set.t
     and type t = (elt, elt_comparator_witness) Set.t
     and type tree = (elt, elt_comparator_witness) Set.Using_comparator.Tree.t
end

(** Does this already exist somewhere? I could not find it… *)
module MakeSetCreators0(M: Comparator.S): Creators'0 with type elt = M.t and type elt_comparator_witness = M.comparator_witness = struct
  type ('a, 'cmp) set = ('a, 'cmp) Set.t
  type t = (M.t, M.comparator_witness) Set.t
  type tree = (M.t, M.comparator_witness) Set.Using_comparator.Tree.t
  type elt = M.t
  type elt_comparator_witness = M.comparator_witness

  let empty = Set.empty (module M)
  let singleton = Set.singleton (module M)
  let union_list = Set.union_list (module M)
  let of_list = Set.of_list (module M)
  let of_sequence = Set.of_sequence (module M)
  let of_array = Set.of_array (module M)
  let of_sorted_array = Set.of_sorted_array (module M)
  let of_sorted_array_unchecked = Set.of_sorted_array_unchecked (module M)
  let of_increasing_iterator_unchecked = Set.of_increasing_iterator_unchecked (module M)
  let stable_dedup_list = Set.stable_dedup_list (module M)
  let map (type a) (s: (a,_) Set.t) ~(f:a -> elt): t = Set.map (module M) s ~f
  let filter_map (type a) (s: (a,_) Set.t) ~(f:a -> elt option): t =
    Set.filter_map (module M) s ~f
  let of_tree = Set.Using_comparator.of_tree ~comparator:M.comparator
end
