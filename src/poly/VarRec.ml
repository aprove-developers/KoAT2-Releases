open! OurBase

module Inner = struct
  type sort = Real | Int [@@deriving eq, ord, sexp]

  type t =
    | Var of String.t
    | Helper of sort * int
    | Argument of int
    | Recursion of Location.t * String.t * Polynomials.Polynomial.t
  [@@deriving eq, ord, sexp]

  let ( =~= ) = equal

  let of_string str =
    match String.chop_prefix str ~prefix:"Temp_Int_" with
    | Some istr -> Helper (Int, Int.of_string istr)
    | None -> (
        match String.chop_prefix str ~prefix:"Temp_Real_" with
        | Some rstr -> Helper (Real, Int.of_string rstr)
        | None -> (
            match String.chop_prefix str ~prefix:"Arg_" with
            | Some astr -> Argument (Int.of_string astr)
            | None -> Var str))


  let hash = Hashtbl.hash
  let mk_helper domain n = Helper (domain, n)

  (** TODO fix this. *)
  let to_string ?(pretty = false) ?(to_file = false) =
    if to_file then
      function
      | Var str -> str
      | Helper (Real, i) -> "TempReal" ^ Int.to_string i
      | Helper (Int, i) -> "TempInt" ^ Int.to_string i
      | Argument i -> "Arg" ^ Int.to_string i
      | Recursion (loc, var, polynom) ->
          "Rec" ^ Location.to_string loc ^ "_" ^ var ^ "_" ^ Polynomials.Polynomial.to_string polynom
    else if pretty then
      function
      | Var str -> str
      | Helper (Real, i) -> "Temp_Real" ^ Util.natural_to_subscript i
      | Helper (Int, i) -> "Temp_Int" ^ Util.natural_to_subscript i
      | Argument i -> "X" ^ Util.natural_to_subscript i
      | Recursion (loc, var, polynom) ->
          "Rec" ^ Location.to_string loc ^ "_" ^ var ^ "_" ^ Polynomials.Polynomial.to_string polynom
    else
      function
      | Var str -> str
      | Helper (Real, i) -> "Temp_Real_" ^ Int.to_string i
      | Helper (Int, i) -> "Temp_Int_" ^ Int.to_string i
      | Argument i -> "Arg_" ^ Int.to_string i
      | Recursion (loc, var, polynom) ->
          "Rec_" ^ Location.to_string loc ^ "_" ^ var ^ "_" ^ Polynomials.Polynomial.to_string polynom


  let counter = ref 0
  let args = Sequence.unfold ~init:0 ~f:(fun i -> Some (Argument i, i + 1))

  (* TODO Use unique from batteries because of thread safety *)
  let fresh_id domain () =
    incr counter;
    Helper (domain, !counter)


  let fresh_ids domain n = Sequence.take (Sequence.uniter (fresh_id domain)) n
  let fresh_id_list domain n = Sequence.to_list (fresh_ids domain n)

  let is_integral = function
    | Var _ -> true
    | Argument _ -> true
    | Helper (Int, _) -> true
    | Helper (Real, _) -> false
    | _ -> false


  let is_real = function
    | Var _ -> false
    | Argument _ -> false
    | Helper (Int, _) -> false
    | Helper (Real, _) -> true
    | _ -> false


  (**returns true if the variable represents real numbers*)
  let is_helper var =
    match var with
    | Var _ -> false
    | Argument _ -> false
    | Helper _ -> true
    | _ -> false


  let is_rec var =
    match var with
    | Recursion (_, _, _) -> true
    | _ -> false


  let to_var = function
    | Var str -> Var.Var str
    | Helper (Real, i) -> Var.Helper (Real, i)
    | Helper (Int, i) -> Var.Helper (Int, i)
    | Argument i -> Var.Argument i
    | Recursion (_, _, _) -> raise (Invalid_argument "Recursive variable cannot be converted to Var.t")
end

include Inner

let of_var = function
  | Var.Var str -> Var str
  | Var.Helper (Real, i) -> Helper (Real, i)
  | Var.Helper (Int, i) -> Helper (Int, i)
  | Var.Argument i -> Argument i


let rename m v = of_var (RenameMap.find (to_var v) m ~default:(to_var v))

let vars var =
  match var with
  | Recursion _ -> VarSet.empty
  | x -> VarSet.singleton (to_var x)


include Comparator.Make (Inner)
