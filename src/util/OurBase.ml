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
let identity = fun x -> x

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

module type SetCreators'0 = sig
  type elt
  type comparator_witness
  include Set.Creators0
    with type elt := elt
     and type comparator_witness := comparator_witness
     and type ('a, 'cmp) set = ('a, 'cmp) Set.t
     and type t = (elt, comparator_witness) Set.t
     and type tree = (elt, comparator_witness) Set.Using_comparator.Tree.t
end

module type MapCreators'1 = sig
  type key
  type comparator_witness
  include Map.Creators1
    with type key := key
    and type comparator_witness := comparator_witness
    and type 'a t = (key,'a,comparator_witness) Base.Map.t
    and type 'a tree = (key, 'a, comparator_witness) Map.Using_comparator.Tree.t
end

(** Does this already exist somewhere? I could not find it… *)
module MakeSetCreators0(M: Comparator.S): SetCreators'0 with type elt = M.t and type comparator_witness = M.comparator_witness = struct
  type ('a, 'cmp) set = ('a, 'cmp) Set.t
  type t = (M.t, M.comparator_witness) Set.t
  type tree = (M.t, M.comparator_witness) Set.Using_comparator.Tree.t
  type elt = M.t
  type comparator_witness = M.comparator_witness

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

module MakeMapCreators1(M: Comparator.S): MapCreators'1 with type key = M.t = struct
  type key = M.t
  type comparator_witness = M.comparator_witness
  type 'a t = (key, 'a, comparator_witness) Map.t
  type 'a tree =  (key,'a,comparator_witness) Map.Using_comparator.Tree.t

  let empty = Map.empty (module M)
  let singleton k v = Map.singleton (module M) k v
  let map_keys m = Map.map_keys (module M) m
  let map_keys_exn m = Map.map_keys_exn (module M) m
  let of_alist l = Map.of_alist (module M) l
  let of_alist_or_error l = Map.of_alist_or_error (module M) l
  let of_alist_exn l = Map.of_alist_exn (module M) l
  let of_alist_multi l = Map.of_alist_multi (module M) l
  let of_alist_fold l = Map.of_alist_fold (module M) l
  let of_alist_reduce l = Map.of_alist_reduce (module M) l
  let of_sorted_array arr = Map.of_sorted_array (module M) arr
  let of_sorted_array_unchecked arr = Map.of_sorted_array_unchecked (module M) arr
  let of_increasing_iterator_unchecked ~len ~f = Map.of_increasing_iterator_unchecked (module M) ~len ~f
  let of_increasing_sequence seq = Map.of_increasing_sequence (module M) seq
  let of_sequence seq = Map.of_sequence (module M) seq
  let of_sequence_or_error seq = Map.of_sequence_or_error (module M) seq
  let of_sequence_exn seq = Map.of_sequence_exn (module M) seq
  let of_sequence_multi seq = Map.of_sequence_multi (module M) seq
  let of_sequence_fold seq = Map.of_sequence_fold (module M) seq
  let of_sequence_reduce seq = Map.of_sequence_reduce (module M) seq
  let of_iteri ~iteri = Map.of_iteri (module M) ~iteri
  let of_iteri_exn ~iteri = Map.of_iteri_exn (module M) ~iteri
  let of_tree tree = Map.of_tree (module M) tree
end
