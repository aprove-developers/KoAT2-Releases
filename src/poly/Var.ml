open Batteries

type sort =
  | Real
  | Int [@@deriving eq, ord]

type t =
  | Var of String.t
  | Helper of sort*int
  | Argument of int [@@deriving eq, ord]

let (=~=) = equal

let of_string str =
  if String.starts_with str "Temp_Int_" then
    Helper (Int,(
        str
        |> String.lchop ~n:9
        |> Int.of_string
      ))
  else if String.starts_with str "Temp_Real_" then
    Helper (Real,(
        str
        |> String.lchop ~n:10
        |> Int.of_string
      ))
  else if String.starts_with str "Arg_" then
    Argument
        (str
        |> String.lchop ~n:4
        |> Int.of_string)
  else
    Var str

let hash = Hashtbl.hash

let mk_helper domain n = Helper (domain, n)

(** TODO fix this. *)
let to_string ?(pretty = false) ?(to_file = false) =
  if to_file then
  function
    | Var str -> str
    | Helper (Real,i) -> "TempReal" ^ (String.of_int i)
    | Helper (Int,i) -> "TempInt" ^ (String.of_int i)
    | Argument i -> "Arg" ^ (String.of_int i)
  else if pretty then
  function
    | Var str -> str
    | Helper (Real,i) -> "Temp_Real" ^ (Util.natural_to_subscript i)
    | Helper (Int,i) -> "Temp_Int" ^ (Util.natural_to_subscript i)
    | Argument i -> "X" ^ (Util.natural_to_subscript i)
  else
  function
    | Var str -> str
    | Helper (Real,i) -> "Temp_Real_" ^ (String.of_int i)
    | Helper (Int,i) -> "Temp_Int_" ^ (String.of_int i)
    | Argument i -> "Arg_" ^ (String.of_int i)

let counter = ref 0

let args =
  LazyList.seq 0 (fun x -> x+1) (const true)
  |> LazyList.map (fun i -> Argument i)

(* TODO Use unique from batteries because of thread safety *)
let fresh_id domain () =
  incr counter;
  Helper (domain, !counter)

let fresh_ids domain n =
  Enum.take n (Enum.from (fresh_id domain))

let fresh_id_list domain n =
  List.of_enum (fresh_ids domain n)

let is_integral = function
  | Var _           -> true
  | Argument _      -> true
  | Helper (Int,_)  -> true
  | Helper (Real,_) -> false

let is_real = function
  | Var _ -> false
  | Argument _ -> false
  | Helper (Int,_) -> false
  | Helper (Real,_) -> true

(**returns true if the variable represents real numbers*)
let is_helper var =
  match var with
  |Var _ -> false
  |Argument _ -> false
  |Helper _ -> true
