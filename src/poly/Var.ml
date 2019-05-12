open Batteries

type sort =
  | Real
  | Int [@@deriving eq, ord]

type substitution_kind =
  NonProbabilistic
  | Probabilistic [@@deriving eq, ord]

let show_substitution_kind = function
  | NonProbabilistic -> "NonProbabilistic"
  | Probabilistic    -> "Probabilistic"

type t =
  | Var of substitution_kind*String.t
  | Helper of sort*int
  | Argument of int [@@deriving eq, ord]

let (=~=) = equal

let of_string str =
  if String.starts_with str "$_" then
    Helper (Int,(
        str
        |> String.lchop ~n:2
        |> Int.of_string
      ))
  else if String.starts_with str "@_" then
    Helper (Real,(
        str
        |> String.lchop ~n:2
        |> Int.of_string
      ))
  else if String.starts_with str "Arg_" then
    Argument
        (str
        |> String.lchop ~n:4
        |> Int.of_string)
  else
    if String.contains str ' ' then
      let info = String.split_on_char ' ' str in
      if (List.nth info 0) = "Probabilistic" then
        Var (Probabilistic, str)
      else
        Var (NonProbabilistic, str)
    else
      Var (NonProbabilistic, str)

let hash = Hashtbl.hash

let mk_helper domain n = Helper (domain, n)

let to_string = function
  | Var (kind, str) -> (show_substitution_kind kind)
  | Helper (Real,i) -> "@_" ^ (String.of_int i)
  | Helper (Int,i) -> "$_" ^ (String.of_int i)
  | Argument i -> "Arg_" ^ (String.of_int i)

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

let get_substitution_kind = function
  | Var (Probabilistic, _) -> Probabilistic
  | Helper (Real, _)       -> Probabilistic
  | _                      -> NonProbabilistic

let set_substitution_kind sub_kind = function
  | Var (_, v) -> Var (sub_kind, v)
  | h          -> h