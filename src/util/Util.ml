open Batteries

let get_all xs =
  let combine result maybe = Option.bind maybe (fun x -> Option.map (fun list -> x :: list) result) in
  List.fold_left combine (Some []) xs


(* https://stackoverflow.com/questions/22120765/group-function-in-ocaml *)
let group f l =
  let rec grouping acc = function
    | [] -> acc
    | hd :: tl ->
        let l1, l2 = List.partition (f hd) tl in
        grouping ((hd :: l1) :: acc) l2
  in
  grouping [] l


let max_option greater =
  let f = function
    | None, y -> Some y
    | Some x, y ->
        if greater x y then
          Some x
        else
          Some y
  in
  Enum.fold (curry f) None


let min_option lesser =
  let f = function
    | None, y -> Some y
    | Some x, y ->
        if lesser x y then
          Some x
        else
          Some y
  in
  Enum.fold (curry f) None


let intersection p enum1 enum2 =
  Enum.cartesian_product enum1 enum2 |> Enum.filter (uncurry p) |> Enum.map Tuple2.first


let without p toBeRemoved = Enum.filter (fun v -> not (Enum.exists (p v) (Enum.clone toBeRemoved)))

let powerset (set : 'a Set.t) : 'a Set.t Enum.t =
  let combine (result : 'a Set.t Enum.t) (x : 'a) =
    Enum.append result (Enum.map (fun ys -> Set.add x ys) (Enum.clone result))
  in
  Enum.fold combine (Enum.singleton Set.empty) (Set.enum set)


let find_map f enum =
  try Some (Enum.find_map f enum) with
  | Not_found -> None


let option_to_string content_to_string option =
  let output = IO.output_string () in
  Option.print (fun output a -> IO.nwrite output (content_to_string a)) output option;
  IO.close_out output


let enum_to_string content_to_string enum =
  let output = IO.output_string () in
  (* To prevent cloning issues *)
  let list = List.of_enum enum in
  List.print (fun output varset -> IO.nwrite output (content_to_string varset)) output list;
  IO.close_out output


let sequence_to_string sequence ~f =
  let open OurBase in
  let list = Sequence.to_list @@ Sequence.map ~f sequence in
  "[" ^ String.concat ~sep:"; " list ^ "]"


let memoize cache ~extractor f =
  let g x =
    match Hashtbl.find_option cache (extractor x) with
    | Some y -> y
    | None ->
        let y = f x in
        Hashtbl.add cache (extractor x) y;
        y
  in
  g


let memoize_base_hashtbl cache ~extractor f =
  let g x =
    match Base.Hashtbl.find cache (extractor x) with
    | Some y -> y
    | None ->
        let y = f x in
        Base.Hashtbl.add_exn cache ~key:(extractor x) ~data:y;
        y
  in
  g


(* TODO: Hash a string into an integer. https://stackoverflow.com/questions/2624192/good-hash-function-for-strings *)
let hash str = String.fold_right (fun char res -> (res * 31) + int_of_char char) str 1
let map_maybe f = List.filter_map (Option.map f)

let rec find_fixpoint_mc f x =
  let res = f x in
  if MaybeChanged.has_changed res then
    MaybeChanged.(res >>= find_fixpoint_mc f)
  else
    res


(* Returns the fixpoint of f *)
let find_fixpoint f x = MaybeChanged.unpack (find_fixpoint_mc f x)

let measure_execution_time ?(methodname = "") f =
  let t0 = Unix.gettimeofday () in
  let res = f () in
  Printf.printf "Time of %s: %.2fs\n" methodname (Unix.gettimeofday () -. t0);
  res


let measure_total_execution_time () =
  let total_time : float ref = ref 0.0 in
  let counter : int ref = ref 0 in
  let get_counter description = Printf.printf "%s: %.2fs over %i calls\n" description !total_time !counter in
  let measure_next_execution f =
    let t0 = Unix.gettimeofday () in
    let res = f () in
    total_time := !total_time +. (Unix.gettimeofday () -. t0);
    counter := !counter + 1;
    res
  in
  (measure_next_execution, get_counter)


let subscript = [ "₀"; "₁"; "₂"; "₃"; "₄"; "₅"; "₆"; "₇"; "₈"; "₉" ]
let superscript = [ "⁰"; "¹"; "²"; "³"; "⁴"; "⁵"; "⁶"; "⁷"; "⁸"; "⁹" ]

exception Negative

let rec natural_to_subscript = function
  | n when n < 0 -> raise Negative
  | n when n < 10 -> List.nth subscript n
  | n ->
      if n / 10 == 0 then
        ""
      else
        natural_to_subscript (n / 10) ^ List.nth subscript (n mod 10)


let rec natural_to_superscript = function
  | n when n < 0 -> raise Negative
  | n when n < 10 -> List.nth superscript n
  | n ->
      if n / 10 == 0 then
        ""
      else
        natural_to_superscript (n / 10) ^ List.nth superscript (n mod 10)


let rec read_from_channel inp_chann =
  try
    let next_line = input_line inp_chann in
    next_line ^ "\n" ^ read_from_channel inp_chann
  with
  | End_of_file -> ""


let read_process cmd =
  let chan = Unix.open_process_in cmd in
  read_from_channel chan


let rec iterate_n_times (f : 'a -> 'a) (n : int) (a : 'a) : 'a =
  if n < 0 then
    raise (Invalid_argument "iterate_n_times: Negative Argument")
  else if n = 0 then
    a
  else
    iterate_n_times f (n - 1) (f a)
