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
let to_string ?(to_file = false) =
  if to_file then
  function
    | Var str -> str
    | Helper (Real,i) -> "TempReal" ^ (String.of_int i)
    | Helper (Int,i) -> "TempInt" ^ (String.of_int i)
    | Argument i -> "Arg" ^ (String.of_int i)
  else
  function
    | Var str -> str
    | Helper (Real,i) -> "Temp_Real_" ^ (String.of_int i)
    | Helper (Int,i) -> "Temp_Int_" ^ (String.of_int i)
    | Argument i -> "Arg_" ^ (String.of_int i)

let to_string_index = function
  | Var str -> str
  | Helper (Real,i) -> "Temp_Real" ^ (Util.natural_to_index i)
  | Helper (Int,i) -> "Temp_Int" ^ (Util.natural_to_index i)
  | Argument i -> "X" ^ (Util.natural_to_index i)

let counter = ref 0

(* We have special variables which are arguments of the transition system. Counting starts with 0 to be with KoAT *)
let arg_counter = ref (-1)

(* TODO Use unique from batteries because of thread safety *)
let fresh_id domain () =
  incr counter;
  Helper (domain, !counter)

let fresh_arg () =
  incr arg_counter;
  Argument !arg_counter

let fresh_ids domain n =
  Enum.take n (Enum.from (fresh_id domain))

let fresh_args n =
  Enum.take n (Enum.from (fresh_arg))

let fresh_id_list domain n =
  List.of_enum (fresh_ids domain n)

let fresh_arg_list n =
  List.of_enum (fresh_args n)

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
