(* explicitly import required modules from Base *)
module Array = Base.Array
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
module Type_equal = Base.Type_equal

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

let ( % ) = Fn.compose
let flip = Fn.flip
let uncurry f (a, b) = f a b

let ( |? ) opt def =
  match opt with
  | Some r -> r
  | None -> def


let tap (f : 'a -> unit) (value : 'a) =
  f value;
  value


let const x _ = x
let identity x = x

module Tuple2 = Batteries.Tuple2
module Tuple3 = Batteries.Tuple3
module Tuple4 = Batteries.Tuple4
module Tuple5 = Batteries.Tuple5

module Atomically : sig
  type 'a t

  val create : 'a -> 'a t
  val run_atomically : 'a t -> ('a -> 'b) -> 'b
  val map_atomically : 'a t -> ('a -> 'a) -> 'a t
end = struct
  type 'a t = Mutex.t * 'a

  let create a = (Mutex.create (), a)

  let run_atomically (mutex, a) f =
    Mutex.lock mutex;
    Exn.protect ~f:(fun () -> f a) ~finally:(fun () -> Mutex.unlock mutex)


  let map_atomically (m, a) f = run_atomically (m, a) f |> fun a' -> (m, a')
end

module Unique = struct
  let counter = Atomically.create (ref 0)

  let unique () =
    Atomically.run_atomically counter (fun counter ->
        let id = !counter in
        counter := !counter + 1;
        id)
end

module Comparator = struct
  include Base.Comparator

  let compare_of_comparator comparator = comparator.compare
  let equal_of_comparator comparator a b = comparator.compare a b = 0
end

module List = struct
  include Base.List

  (** Like zip2 but truncate the longer list if both are of uneven length *)
  let rec zip_truncate l1 l2 =
    match (l1, l2) with
    | x :: xs, y :: ys -> (x, y) :: zip_truncate xs ys
    | _ -> []


  (** Like map2 but truncate the longer list if both are of uneven length *)
  let rec map2_truncate l1 l2 ~f =
    match (l1, l2) with
    | x :: xs, y :: ys -> f x y :: map2_truncate xs ys ~f
    | _ -> []


  let rec modify_at i ~f l =
    match l with
    | x :: xs when i = 0 -> f x :: xs
    | x :: xs when i > 0 -> x :: modify_at (i - 1) ~f xs
    | _ -> raise (Invalid_argument "List.modify_at: Index does not exist")
end

module Sequence = struct
  include Base.Sequence

  (** Counterpart to iter. E.g. construct a sequence by pulling from a function *)
  let uniter f = unfold ~init:() ~f:(fun () -> Some (f (), ()))
end

module Set = struct
  include Base.Set

  (** All subsets of the given specific size *)
  let combinations (type a cmp) (module M : Comparator.S with type t = a and type comparator_witness = cmp)
      (set : (a, cmp) t) (k : int) =
    let empty_set = empty (module M) in
    let arr = to_array set in
    let set_length = Array.length arr in
    let next (i, rem_choices, state_stack, curr_set) =
      if i < set_length then
        let rem_elements = set_length - i in
        if rem_choices = rem_elements then
          let remaining_set =
            Sequence.range i set_length |> Sequence.map ~f:(Array.get arr) |> of_sequence (module M)
          in
          Sequence.Step.Skip { state = (i + 1, rem_choices - 1, state_stack, union curr_set remaining_set) }
        else if rem_choices > 0 then
          Sequence.Step.Skip
            {
              state =
                ( i + 1,
                  rem_choices - 1,
                  (i, rem_choices, curr_set) :: state_stack,
                  add curr_set (Array.get arr i) );
            }
        else
          Sequence.Step.Skip { state = (set_length, rem_choices, state_stack, curr_set) }
      else if i = set_length then
        match state_stack with
        | [] -> Sequence.Step.Yield { value = curr_set; state = (i + 1, -1, [], empty_set) }
        | prev_state :: rem ->
            let i', rem_choices', curr_set' = prev_state in
            Sequence.Step.Yield { value = curr_set; state = (i' + 1, rem_choices', rem, curr_set') }
      else
        Sequence.Step.Done
    in
    Sequence.unfold_step ~init:(0, k, [], empty_set) ~f:next


  (** The powerset in ascending order of subset cardinality *)
  let powerset (type a cmp) (module M : Comparator.S with type t = a and type comparator_witness = cmp)
      (set : (a, cmp) t) =
    Sequence.range ~stop:`inclusive 0 (length set)
    |> Sequence.map ~f:(combinations (module M) set)
    |> Sequence.join
end

module Map = struct
  include Base.Map

  let find_default m ~default key = Option.value ~default (find m key)
end

module Hashtbl = struct
  include Base.Hashtbl

  (** Adds the key or overwrites it if already present *)
  let add_or_overwrite m ~key ~data = change m key ~f:(fun o -> Some data)

  let to_sequence tbl = Sequence.of_list (to_alist tbl)
end

(** Useful for constructing sets of sets *)
module DerivedComparatorOnSets = Comparator.Derived_phantom (struct
  type ('a, 'b) t = ('a, 'b) Set.t

  let sexp_of_t (type a) (sexp_of_a : a -> Sexp.t) (t : (a, 'b) t) : Sexp.t =
    let module Sexp_of = struct
      type t = a

      let sexp_of_t = sexp_of_a
    end in
    Set.sexp_of_m__t (module Sexp_of) t


  let compare _ = Set.compare_direct
end)

module DerivedComparatorOnMaps = Comparator.Derived2_phantom (struct
  type ('a, 'b, 'c) t = ('a, 'b, 'c) Map.t

  let sexp_of_t (type a b) (sexp_of_a : a -> Sexp.t) (sexp_of_b : b -> Sexp.t) (t : (a, b, 'c) t) : Sexp.t =
    let module Sexp_of = struct
      type t = a

      let sexp_of_t = sexp_of_a
    end in
    Map.sexp_of_m__t (module Sexp_of) sexp_of_b t


  let compare _ v_compare = Map.compare_direct v_compare
end)

(** Lexicographic comparisons of tuples *)
module TupleComparator = Comparator.Derived2 (struct
  type ('a, 'b) t = 'a * 'b [@@deriving sexp_of]

  let compare a_compare b_compare (a1, b1) (a2, b2) =
    let ares = a_compare a1 a2 in
    if ares <> 0 then
      ares
    else
      b_compare b1 b2
end)

module MakeComparatorForTuples (M1 : Comparator.S) (M2 : Comparator.S) :
  Comparator.S
    with type t = M1.t * M2.t
     and type comparator_witness =
      (M1.comparator_witness, M2.comparator_witness) TupleComparator.comparator_witness = struct
  type t = M1.t * M2.t
  type comparator_witness = (M1.comparator_witness, M2.comparator_witness) TupleComparator.comparator_witness

  let comparator = TupleComparator.comparator M1.comparator M2.comparator
end

module MakeComparatorForSet (M : Comparator.S) :
  Comparator.S
    with type t = (M.t, M.comparator_witness) Set.t
     and type comparator_witness = M.comparator_witness DerivedComparatorOnSets.comparator_witness = struct
  type t = (M.t, M.comparator_witness) Set.t
  type comparator_witness = M.comparator_witness DerivedComparatorOnSets.comparator_witness

  let comparator = DerivedComparatorOnSets.comparator M.comparator
end

module type SetCreators'0 = sig
  type elt
  type elt_comparator_witness

  include
    Set.Creators_generic
      with type 'e elt := elt
       and type 'c cmp := elt_comparator_witness
       and type ('a, 'cmp) t := (elt, elt_comparator_witness) Set.t
       and type ('a, 'cmp) set := ('a, 'cmp) Set.t
       and type ('a, 'cmp) tree := (elt, elt_comparator_witness) Set.Using_comparator.Tree.t
       and type ('a, 'cmp, 'z) create_options := 'z

  (* TODO: Get rid of this helper type as it can complicate error messages *)
  type t = (elt, elt_comparator_witness) Set.t

  val powerset : (elt, elt_comparator_witness) Set.t -> (elt, elt_comparator_witness) Set.t Sequence.t

  val combinations :
    (elt, elt_comparator_witness) Set.t -> int -> (elt, elt_comparator_witness) Set.t Sequence.t
end

module type MapCreators'1 = sig
  type key
  type key_comparator_witness

  include
    Map.Creators_generic
      with type 'k key := key
       and type 'c cmp := key_comparator_witness
       and type ('k, 'v, 'c) t := (key, 'v, key_comparator_witness) Map.t
       and type ('k, 'v, 'cmp) tree := (key, 'v, key_comparator_witness) Map.Using_comparator.Tree.t
       and type ('a, 'cmp, 'z) create_options := 'z
       and type ('a, 'cmp, 'z) access_options := 'z

  (* TODO: Get rid of this helper type as it can complicate error messages *)
  type 'v t = (key, 'v, key_comparator_witness) Map.t
end

(** Does this already exist somewhere? I could not find it… *)
module MakeSetCreators0 (M : Comparator.S) :
  SetCreators'0 with type elt = M.t and type elt_comparator_witness = M.comparator_witness = struct
  type elt = M.t
  type elt_comparator_witness = M.comparator_witness
  type t = (elt, elt_comparator_witness) Set.t

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
  let map s ~f = Set.map (module M) s ~f
  let filter_map s ~f = Set.filter_map (module M) s ~f
  let of_tree = Set.Using_comparator.of_tree ~comparator:M.comparator
  let powerset = Set.powerset (module M)
  let combinations = Set.combinations (module M)
end

module MakeMapCreators1 (M : Comparator.S) :
  MapCreators'1 with type key = M.t and type key_comparator_witness = M.comparator_witness = struct
  type key = M.t
  type key_comparator_witness = M.comparator_witness
  type 'v t = (key, 'v, key_comparator_witness) Map.t

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
  let transpose_keys m = Map.transpose_keys (module M) m
  let of_list_with_key l ~get_key = Map.of_list_with_key (module M) l ~get_key
  let of_list_with_key_multi l ~get_key = Map.of_list_with_key_multi (module M) l ~get_key
  let of_list_with_key_exn l ~get_key = Map.of_list_with_key_exn (module M) l ~get_key
  let of_list_with_key_or_error l ~get_key = Map.of_list_with_key_or_error (module M) l ~get_key
end
