open OurBase
(** A type that is wrapped by the MaybeChanged monad represents a value that might change during a computation.
    Once it has changed it will be forever marked as changed, but can also be further be manipulated. *)

type status = Changed | Same
type 'a t = status * 'a

let return subject = (Same, subject)
let pure = return

let ( >>= ) maybe f =
  match maybe with
  | Changed, subject ->
      ( Changed,
        let _, s = f subject in
        s )
  | Same, subject -> f subject


let bind = ( >>= )
let flat_map f maybe = maybe >>= f

let map f = function
  | status, subject -> (status, f subject)


let unpack = Tuple2.second

let has_changed = function
  | Changed, _ -> true
  | _ -> false


let if_changed f = function
  | Changed, subject -> (Changed, f subject)
  | Same, subject -> (Same, subject)


let changed subject = (Changed, subject)
let same subject = (Same, subject)

let fold_sequence ~(f : 'a -> 'b -> 'a t) ~(init : 'a) (seq : 'b Sequence.t) : 'a t =
  Sequence.fold
    ~f:(fun maybe_changed element -> maybe_changed >>= fun subject -> f subject element)
    ~init:(same init) seq


let fold (f : 'a -> 'b -> 'a t) (subject : 'a) (list : 'b List.t) : 'a t =
  List.fold list
    ~f:(fun maybe_changed element -> maybe_changed >>= fun subject -> f subject element)
    ~init:(same subject)
