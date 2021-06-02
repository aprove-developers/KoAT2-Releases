open Batteries

let get_all xs =
  let combine result maybe =
    Option.bind maybe (fun x -> Option.map (fun list -> x :: list) result) in
  List.fold_left combine (Some []) xs

let max_option greater =
  let f = function
    | (None, y) -> Some y
    | (Some x, y) -> if greater x y then Some x else Some y in
  Enum.fold (curry f) None

let min_option lesser =
  let f = function
    | (None, y) -> Some y
    | (Some x, y) -> if lesser x y then Some x else Some y in
  Enum.fold (curry f) None

let intersection p enum1 enum2 =
  Enum.cartesian_product enum1 enum2
  |> Enum.filter (uncurry p)
  |> Enum.map Tuple2.first

let without p toBeRemoved =
  Enum.filter (fun v -> not (Enum.exists (p v) (Enum.clone toBeRemoved)))

let powerset (set: 'a Set.t): ('a Set.t) Enum.t =
  let combine (result: ('a Set.t) Enum.t) (x: 'a) = Enum.append result (Enum.map (fun ys -> Set.add x ys) (Enum.clone result)) in
  Enum.fold combine (Enum.singleton Set.empty) (Set.enum set)

let find_map f enum =
  try
    Some (Enum.find_map f enum)
  with
    Not_found -> None

let option_to_string content_to_string option =
  let output = IO.output_string () in
  Option.print (fun output a -> IO.nwrite output (content_to_string a)) output option;
  IO.close_out output

let enum_to_string content_to_string enum =
  let output = IO.output_string () in
  (** To prevent cloning issues *)
  let list = List.of_enum enum in
  List.print (fun output varset -> IO.nwrite output (content_to_string varset)) output list;
  IO.close_out output

let memoize ~extractor f =
  let cache = Hashtbl.create 10 in
  let g x =
    match Hashtbl.find_option cache (extractor x) with
    | Some y -> y
    | None ->
       let y = f x in
       Hashtbl.add cache (extractor x) y;
       y
  in g

let cache ~extractor = object
  val cache = Hashtbl.create 10

  method add f x =
    match Hashtbl.find_option cache (extractor x) with
    | Some y -> y
    | None ->
       let y = f x in
       Hashtbl.add cache (extractor x) y;
       y

  method clear = Hashtbl.clear cache
  end

(* TODO: Hash a string into an integer. https://stackoverflow.com/questions/2624192/good-hash-function-for-strings *)
let hash str =
 String.fold_right (fun char res -> res * 31 + (int_of_char char)) str 1

(** Returns true iff s2 is contained in s1. *)
let contains s1 s2 =
    let re = Str.regexp_string s2
    in
        try ignore (Str.search_forward re s1 0); true
        with Not_found -> false

let cat_maybes l=
  List.map Option.get (List.filter Option.is_some l)

let cat_maybes_enum e =
  Enum.map Option.get (Enum.filter Option.is_some e)

let measure_execution_time f =
  let t0 = Unix.gettimeofday () in
  let res = f () in
  Printf.printf "Time %.2fs\n" (Unix.gettimeofday () -. t0);
  res

let measure_total_execution_time () =
  let total_time: float ref = ref 0.0 in
  let counter: int ref = ref 0 in
  let get_counter description =
    Printf.printf "%s: %.2fs over %i calls\n" (description) (!total_time) (!counter)
  in
  let measure_next_execution f =
    let t0 = Unix.gettimeofday () in
    let res = f () in
    total_time := !total_time +. (Unix.gettimeofday () -. t0);
    counter := !counter + 1;
    res
  in
  measure_next_execution, get_counter
