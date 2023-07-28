
(* explicitly import required modules from Base *)
module Array = Base.Array
module Comparator = Base.Comparator
module Exn = Base.Exn
module Float = Base.Float
module Fn = Base.Fn
module Int = Base.Int
module Option = Base.Option
module Poly = Base.Poly
module Sexp = Base.Sexp
module Sexpable = Base.Sexpable
module Stack = Base.Stack
module String = Base.String

let bool_of_sexp = Base.bool_of_sexp
let float_of_sexp = Base.float_of_sexp
let int_of_sexp = Base.int_of_sexp
let string_of_sexp = Base.string_of_sexp

let sexp_of_bool = Base.sexp_of_bool
let sexp_of_float = Base.sexp_of_float
let sexp_of_int = Base.sexp_of_int
let sexp_of_string = Base.sexp_of_string

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

module Atomically: sig
  type 'a t
  val create: 'a -> 'a t
  val run_atomically: 'a t -> ('a -> 'b) -> 'b
end = struct
  type 'a t = Mutex.t * 'a

  let create a = Mutex.create (), a

  let run_atomically (mutex,a) f =
    Mutex.lock mutex;
    Exn.protect ~f:(fun () -> f a) ~finally:(fun () -> Mutex.unlock mutex)
end

module Unique = struct
  let counter = Atomically.create (ref 0)
  let unique () = Atomically.run_atomically counter (fun counter ->
      let id = !counter in
      counter := !counter+1;
      id
    )
end

module List = struct
  include Base.List

  (** Like zip2 but truncate the longer list if both are of uneven length *)
  let rec zip_truncate l1 l2 = match (l1,l2) with
    | (x::xs,y::ys) -> (x,y) :: zip_truncate xs ys
    | _           -> []

  (** Like map2 but truncate the longer list if both are of uneven length *)
  let rec map2_truncate l1 l2 ~f = match (l1,l2) with
    | (x::xs,y::ys) -> f x y :: map2_truncate xs ys ~f
    | _ -> []

  let rec modify_at i ~f l = match l with
    | (x::xs) when i = 0 -> f x :: xs
    | (x::xs) when i > 0 -> x :: modify_at (i-1) ~f xs
    | _ -> raise (Invalid_argument "List.modify_at: Index does not exist")
end

module Sequence = struct
  include Base.Sequence

  (** Counterpart to iter. E.g. construct a sequence by pulling from a function *)
  let uniter f =
    unfold ~init:() ~f:(fun () -> Some (f (), ()))
end

module Set = struct
  include Base.Set

  let powerset (type a cmp) (module M: Comparator.S with type t = a and type comparator_witness = cmp) (set: (a,cmp) t) =
    let combine result x =
      Sequence.append result (Sequence.map ~f:(fun ys -> Base.Set.add ys x) result)
    in
    Base.Sequence.fold ~f:combine ~init:(Sequence.singleton (empty (module M))) (Base.Set.to_sequence set)
end

module Map = struct
  include Base.Map
  let find_default m ~default key = Option.value ~default (find m key)

  (** Adds the key or overwrites it if already present *)
  let add_or_overwrite m ~key ~data = change m key ~f:(fun o -> Some data)
end

module Hashtbl = struct
  include Base.Hashtbl

  (** Adds the key or overwrites it if already present *)
  let add_or_overwrite m ~key ~data = change m key ~f:(fun o -> Some data)

  let to_sequence tbl = Sequence.of_list (to_alist tbl)
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
