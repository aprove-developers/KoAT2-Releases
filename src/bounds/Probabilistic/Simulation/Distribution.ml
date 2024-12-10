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
end

include Inner
module Monad = Monad.Make (Inner)

let of_tuples values =
  let open OurRational in
  let rec aux p = function
    | [] -> failwith ""
    | [ (p', x) ] -> Leaf x
    | (p', x) :: xs -> Choice (p' / p, lazy (Leaf x), lazy (aux OurRational.(one - p') xs))
  in
  aux one values


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
