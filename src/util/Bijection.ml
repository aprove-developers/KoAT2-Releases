open OurBase

type ('a, 'b, 'a_cmp, 'b_cmp) t = ('a,'b,'a_cmp) Map.t * ('b,'a,'b_cmp) Map.t


let of_sequence (type a b a_cmp b_cmp) (a_comparator_module: (a,a_cmp) Comparator.Module.t) (b_comparator_module: (b,b_cmp) Comparator.Module.t) (seq: (a * b) Sequence.t): (a,b,a_cmp,b_cmp) t =
  let seq_rev = Base.Sequence.map ~f:Tuple2.swap seq in
  Base.Map.of_sequence_exn a_comparator_module seq, Base.Map.of_sequence_exn b_comparator_module seq_rev

(* let of_sequence_rev = *)
(*   of_sequence % Sequence.map ~f:(fun(a,b) -> b,a) *)

let amap (am, _) = am
let bmap (_, bm) = bm

let to_sequence t =
  Map.to_sequence (amap t)

let to_sequence_rev t =
  Map.to_sequence (bmap t)

let finda t a =
  Map.find (amap t) a

let findb t b =
 Map.find (bmap t) b

let find = finda

let finda_exn t a = Map.find_exn (amap t) a
let findb_exn t b = Map.find_exn (bmap t) b
let find_exn = finda_exn

let keys (type a b a_cmp b_cmp) (t: (a,b,a_cmp,b_cmp) t) =
  Map.keys (amap t)

let values (type a b a_cmp b_cmp) (t: (a,b,a_cmp,b_cmp) t) =
  Map.keys (bmap t)

let removea a (amap,bmap) =
  match Map.find amap a with
  | Some b -> Map.remove amap a, Map.remove bmap b
  | None   -> amap,bmap

let removeb b (amap,bmap) =
  match Map.find bmap b with
  | Some a -> Map.remove amap a, Map.remove bmap b
  | None   -> amap,bmap

let remove = removea

let add a b (amap,bmap) =
  Map.add_exn ~key:a ~data:b amap, Map.add_exn ~key:b ~data:a bmap


module Make(A: Comparator.S)(B: Comparator.S) = struct

  type ('a,'b,'a_cmp,'b_cmp) outer_t = ('a,'b,'a_cmp,'b_cmp) t
  type t = (A.t,B.t,A.comparator_witness,B.comparator_witness) outer_t

  let of_sequence seq: t =
    let seq_rev = Base.Sequence.map ~f:Tuple2.swap seq in
    Base.Map.of_sequence_exn (module A) seq, Base.Map.of_sequence_exn (module B) seq_rev

  let of_sequence_rev =
    of_sequence % Sequence.map ~f:(fun(a,b) -> b,a)

end
