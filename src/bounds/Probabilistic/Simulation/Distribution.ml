module Inner = struct
  type 'a t = Leaf of 'a | Choice of OurRational.t * 'a t Lazy.t * 'a t Lazy.t

  let pure a = Leaf a

  let rec map (f : 'a -> 'b) (a : 'a t) =
    match a with
    | Leaf x -> Leaf (f x)
    | Choice (p, l, r) -> Choice (p, Lazy.map (map f) l, Lazy.map (map f) r)


  let rec bind (a : 'a t) (f : 'a -> 'b t) =
    match a with
    | Leaf a -> f a
    | Choice (p, l, r) -> Choice (p, Lazy.map (fun x -> bind x f) l, Lazy.map (fun x -> bind x f) r)


  let choice p a b = Choice (p, Lazy.from_fun a, Lazy.from_fun b)
end

include Inner
module Monad = Monad.Make (Inner)
open OurBase
open Monad

let of_tuples values =
  let open OurRational in
  let rec aux p seq =
    match Sequence.length seq with
    | 0 -> failwith "Empty sequence"
    | 1 -> Leaf (Sequence.hd_exn seq |> snd)
    | _ ->
        let p', x = Sequence.hd_exn seq in
        let tail = Sequence.tl_eagerly_exn seq in
        choice (p' / p) (fun _ -> pure x) (fun _ -> aux (one - p') tail)
  in
  aux one values


let rec replicateM n m =
  let open OurInt in
  if n <= zero then
    pure Fn.id
  else
    m >>= fun f -> map (fun acc x -> acc (f x)) (replicateM (n - one) m)


let uniform values =
  let rec aux n = function
    | [] -> failwith ""
    | [ x ] -> Leaf x
    | x :: xs -> Choice (OurRational.(one / n), lazy (Leaf x), lazy (aux OurRational.(n - one) xs))
  in
  let n = List.length values |> OurRational.of_int in
  aux n values


let uniform_from_to a b =
  if a > b then
    invalid_arg "uniform_from_to: lower bound a must be less than or equal to upper bound b"
  else
    let values =
      Base.List.init (OurInt.(b - a) |> OurInt.to_int |> ( + ) 1) ~f:(fun x -> OurInt.(a + OurInt.of_int x))
    in
    uniform values


let rec geo p =
  let open OurInt in
  choice p (fun () -> pure one) (fun () -> map (( + ) one) (geo p))


let binomial n p =
  let open OurInt in
  replicateM n (choice p (fun () -> pure (( + ) one)) (fun () -> pure (( + ) zero))) |> map (fun f -> f zero)


let hyper_geometric bigN k (n : OurInt.t) : OurInt.t t =
  let open OurInt in
  let rec aux (bigN : OurInt.t) (k : OurInt.t) (n : OurInt.t) (x : OurInt.t) =
    if n <= one then
      choice OurRational.(of_ourint k / of_ourint bigN) (fun _ -> pure (x + one)) (fun _ -> pure x)
    else
      choice
        OurRational.(of_ourint k / of_ourint bigN)
        (fun _ -> aux (bigN - one) (k - one) (n - one) (x + one))
        (fun _ -> aux (bigN - one) k (n - one) x)
  in
  aux bigN k n zero


let rec to_string ~f dist =
  match dist with
  | Leaf a -> Printf.sprintf "Leaf (%s)" (f a)
  | Choice (r, (lazy left), (lazy right)) ->
      Printf.sprintf "Choice (%s, %s, %s)" (OurRational.to_string r)
        (left |> to_string ~f)
        (right |> to_string ~f)


let iter_n n ~f =
  let rec aux n f p = function
    | Leaf u -> f p u
    | Choice (p', (lazy left), (lazy right)) ->
        if n == 0 then
          ()
        else (
          aux (n - 1) f OurRational.(p * p') left;
          aux (n - 1) f OurRational.(p * (one - p')) right)
  in
  aux n f (OurRational.of_int 1)


let to_list dist =
  let open OurRational in
  let rec aux p dist =
    match dist with
    | Leaf u -> [ (u, p) ]
    | Choice (p', (lazy left), (lazy right)) -> List.append (aux (p * p') left) (aux (p * (one - p')) right)
  in
  aux one dist


let to_grouped_list dist ~cmp =
  let open OurRational in
  to_list dist
  |> List.sort_and_group ~compare:(fun (x1, _) (x2, _) -> cmp x1 x2)
  |> List.map ~f:(fun lst ->
         List.fold lst
           ~init:(List.nth_exn lst 0 |> fst, zero)
           ~f:(fun (acc_x, acc_p) (x, p) -> (x, acc_p + p)))
