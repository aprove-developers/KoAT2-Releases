open! OurBase
open Polynomials

module Inner = struct
  type sort = Real | Int [@@deriving eq, ord, sexp]

  type t =
    | Var of String.t
    | Helper of sort * int
    | Argument of int
    | Recursion of Location.t * Var.t * VarMapPoly.map_type
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
      | Recursion (loc, var, map) ->
          Location.to_string loc ^ "[" ^ Var.to_string var ^ " | " ^ VarMapPoly.to_string map ^ "]"
    else if pretty then
      function
      | Var str -> str
      | Helper (Real, i) -> "Temp_Real" ^ Util.natural_to_subscript i
      | Helper (Int, i) -> "Temp_Int" ^ Util.natural_to_subscript i
      | Argument i -> "X" ^ Util.natural_to_subscript i
      | Recursion (loc, var, map) ->
          Location.to_string loc ^ "[" ^ Var.to_string var ^ " | " ^ VarMapPoly.to_string map ^ "]"
    else
      function
      | Var str -> str
      | Helper (Real, i) -> "Temp_Real_" ^ Int.to_string i
      | Helper (Int, i) -> "Temp_Int_" ^ Int.to_string i
      | Argument i -> "Arg_" ^ Int.to_string i
      | Recursion (loc, var, map) ->
          Location.to_string loc ^ "[" ^ Var.to_string var ^ " | " ^ VarMapPoly.to_string map ^ "]"


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


  let to_var_or_tmp = function
    | Var str -> Var.Var str
    | Helper (Real, i) -> Var.Helper (Real, i)
    | Helper (Int, i) -> Var.Helper (Int, i)
    | Argument i -> Var.Argument i
    | Recursion (_, _, _) -> Var.fresh_id Int ()


  let return_loc = function
    | Recursion (l, _, _) -> l
    | _ -> raise (invalid_arg "Non recursive variable do not have return location.")


  let return_var = function
    | Recursion (_, v, _) -> v
    | _ -> raise (invalid_arg "Non recursive variable do not have return location.")


  let update = function
    | Recursion (_, _, u) -> u
    | _ -> raise (invalid_arg "Non recursive variable do not have an update.")
end

include Inner

let of_var = function
  | Var.Var str -> Var str
  | Var.Helper (Real, i) -> Helper (Real, i)
  | Var.Helper (Int, i) -> Helper (Int, i)
  | Var.Argument i -> Argument i


let rename m v =
  let f v = RenameMap.find v m ~default:v in
  match v with
  | Recursion (l, v, map) ->
      Recursion
        ( l,
          f v,
          Map.map map ~f:(Polynomial.rename m) |> VarMapPoly.map_keys_exn ~f:(VarIndeterminate.rename m) )
  | x -> of_var @@ VarIndeterminate.rename m (to_var x)


let vars = function
  | Recursion _ -> VarSet.empty
  | x -> VarSet.singleton (to_var x)


let mk_rec start result patterns target =
  let map_to_arg_vars = Sequence.zip (Sequence.of_list patterns) Var.args |> RenameMap.of_sequence in
  let fill_up_update_arg_vars_up_to_num n update =
    let missing_args =
      Set.diff (VarSet.of_sequence @@ Sequence.take Var.args n) (VarSet.of_list @@ Map.keys update)
    in
    Set.fold ~f:(fun vmap v -> Map.add_exn vmap ~key:v ~data:(Polynomial.of_var v)) missing_args ~init:update
  in
  let update =
    Sequence.of_list target
    |> Sequence.map ~f:(Polynomial.rename map_to_arg_vars)
    |> Sequence.zip Var.args
    |> Map.of_sequence_exn (module Var)
    |> fill_up_update_arg_vars_up_to_num (List.length patterns)
  in
  Recursion (start, result, update)


let dependencies input_vars x = function
  | Recursion (_, _, map) ->
      let rec f contributors non_contributors =
        let xs, ys =
          Set.fold
            ~f:(fun (contr, non_contr) y ->
              if
                Set.exists
                  ~f:(fun x -> Polynomial.vars (Map.find map x |? Polynomial.zero) |> flip Set.mem y)
                  contr
              then
                (Set.add contr y, Set.remove non_contr y)
              else
                (contr, non_contr))
            input_vars ~init:(contributors, non_contributors)
        in
        if Set.equal non_contributors ys then
          contributors
        else
          f xs ys
      in
      f (VarSet.singleton x) (Set.diff input_vars (VarSet.singleton x))
  | x -> VarSet.empty


let remove_non_contributors non_contributors = function
  | Recursion (l, v, map) ->
      let vars = Map.keys map in
      let patterns = List.filter ~f:(Set.mem (Set.diff (VarSet.of_list vars) non_contributors)) vars in
      let assignments = List.map ~f:(Map.find_exn map) patterns in
      mk_rec l v patterns assignments
  | x -> x


include Comparator.Make (Inner)
