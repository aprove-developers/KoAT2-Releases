open! OurBase
open ProgramModules

module ParseSexpListMonad = struct
  type 'a result_type = (Sexp.t List.t * 'a, String.t) Result.t

  module M = Monad.MakeGeneral (struct
    (* TODO store recursive path here for better errors  *)
    type 'a t = Sexp.t List.t -> 'a result_type

    let pure x = fun sexps -> Ok (sexps, x)
    let bind ma f = fun sexps -> Result.bind (ma sexps) (fun (sexps_rem, a) -> f a sexps_rem)
  end)

  let error s = fun sexps -> Error s

  module A = Alternative.Make (struct
    include M

    let empty = error "Empty"

    let ( <|> ) fa1 fa2 =
     fun sexps ->
      match fa1 sexps with
      | Ok a -> Ok a
      | Error e -> fa2 sexps
  end)

  include A
end

open ParseSexpListMonad

(* Small recursive descent parsing library below *)

let rec _sexp_to_string_with_list_structure = function
  | Sexp.Atom s -> s
  | Sexp.List l -> Util.sequence_to_string ~f:_sexp_to_string_with_list_structure (Sequence.of_list l)


let run_parser (p : 'a ParseSexpListMonad.t) sexps : ('a, String.t) Result.t =
  match p sexps with
  | Ok (rem, a) -> Ok a
  | Error s -> Error s


let get_sexps : Sexp.t List.t ParseSexpListMonad.t = fun sexps -> Ok (sexps, sexps)

(* EOS = end of s-expression *)
let generate_error exp = function
  | sexp :: _ -> Error ("Expected " ^ exp ^ " but got " ^ Sexp.to_string sexp)
  | [] -> Error ("Expected " ^ exp ^ " but got " ^ "EOS")


let parse_eos : unit ParseSexpListMonad.t = function
  | [] -> Ok ([], ())
  | sexp :: _ -> Error ("Expected EOS but got " ^ Sexp.to_string sexp)


let parse_atom : string ParseSexpListMonad.t = function
  | Sexp.Atom str :: rem -> Ok (rem, str)
  | sexps -> generate_error "atom" sexps


let parse_atom_sat (p : Char.t -> Bool.t) ?(err = "atom did not match sat") : string ParseSexpListMonad.t =
  let* str = parse_atom in
  if String.for_all ~f:p str then
    pure str
  else
    error (err ^ " got " ^ str)


let parse_opt p = Option.some <$> p <|> pure None

let parse_string str : unit ParseSexpListMonad.t =
  let* str' = parse_atom in
  if String.equal str' str then
    pure ()
  else
    error ("Expected " ^ str ^ " but got " ^ str)


let parse_string_prefix prefix : unit ParseSexpListMonad.t =
  let gen_error sexps = generate_error ("parse_prefix " ^ prefix) sexps in
  function
  | Sexp.Atom str :: rem as sexps -> (
      match String.chop_prefix ~prefix str with
      | Some str_rest -> Ok (Sexp.Atom str_rest :: rem, ())
      | None -> gen_error sexps)
  | sexps -> gen_error sexps


let parse_sexps_from_file filename =
  match Parsexp_io.load (module Parsexp.Many) ~filename with
  | Ok a -> a
  | Error e -> raise (Parsexp.Parse_error.Parse_error e)


(** This is only to be used for testing purposes because it discards the remaining input *)
let _eat_to_eos : unit ParseSexpListMonad.t = fun _ -> Ok ([], ())

let parse_from_uncons (p : Sexp.t * Sexp.t List.t -> 'a result_type) : 'a ParseSexpListMonad.t = function
  | sexp :: rem -> p (sexp, rem)
  | [] -> Error "Unexpected EOS"


let map_from_head (f : Sexp.t -> 'a) : 'a ParseSexpListMonad.t =
  parse_from_uncons (fun (sexp, rem) -> Ok (sexp :: rem, f sexp))


let _get_head : Sexp.t ParseSexpListMonad.t = map_from_head identity

let _print_head =
  let+ h = _get_head in
  Printf.printf "head: %s\n" (_sexp_to_string_with_list_structure h)


let apply_head f : unit ParseSexpListMonad.t = parse_from_uncons (fun (sexp, rem) -> Ok (f sexp :: rem, ()))

let descend_into (p : 'a ParseSexpListMonad.t) : 'a ParseSexpListMonad.t = function
  | (Sexp.List sexps as head) :: rem -> (
      match p sexps with
      | Ok ([], a) -> Ok (rem, a)
      | Ok (rem_descend, a) ->
          Error
            ("From " ^ Sexp.to_string head ^ " we could not parse " ^ Sexp.to_string (Sexp.List rem_descend))
      | Error e -> Error e)
  | sexps -> generate_error "list" sexps


let descend_into_or_lift (p : 'a ParseSexpListMonad.t) : 'a ParseSexpListMonad.t =
  let single_sexp_to_sexp_list = function
    | Sexp.List _ as l -> l
    | Sexp.Atom a -> Sexp.List [ Sexp.Atom a ]
  in
  apply_head single_sexp_to_sexp_list %> descend_into p


(* ARI parsing functions *)

let parse_format : unit ParseSexpListMonad.t =
  descend_into_or_lift @@ (parse_string "format" %> parse_string "LCTRS")


let parse_theory : unit ParseSexpListMonad.t =
  descend_into_or_lift @@ (parse_string "theory" %> parse_string "Ints")


let parse_int = parse_string "Int"
let parse_loc = Location.of_string <$> parse_atom

let parse_loc_and_arity : (Location.t * Int.t) ParseSexpListMonad.t =
  let parse_ty_arity : Int.t ParseSexpListMonad.t =
    let parse_int = 0 <$ parse_int in
    let parse_complex = parse_string "->" %> parse_int %> map List.length (some parse_int) in
    parse_int <|> parse_complex
  in
  let parse_fun_sexp =
    parse_string "fun" %> (Tuple2.make <$> parse_loc <%> descend_into_or_lift parse_ty_arity)
  in
  descend_into_or_lift parse_fun_sexp


let parse_entrypoint : Location.t ParseSexpListMonad.t =
  descend_into_or_lift @@ (parse_string "entrypoint" %> map Location.of_string parse_atom)


type arith_op = PLUS | MINUS | TIMES
type logical_op = GT | GE | EQ | LE | LT | AND | OR | NEQ

let parse_var = Var.of_string <$> parse_atom

let parse_arith ?(var_rename = Map.empty (module Var)) =
  let open Polynomials in
  let rec parse_arith_ () =
    let parse_int =
      let parse_uint =
        let is_digit = function
          | '0' .. '9' -> true
          | _ -> false
        in
        OurInt.of_string <$> parse_atom_sat is_digit
      in
      parse_uint <|> (OurInt.neg <$> parse_string_prefix "-" %> parse_uint) |> map Polynomial.of_constant
    in
    let parse_var =
      let+ v = parse_var in
      Map.find_default var_rename ~default:v v
    in
    let parse_var_poly = Polynomial.of_var <$> parse_var in
    let parse_expr =
      let* op = parse_string "+" $> PLUS <|> (parse_string "-" $> MINUS) <|> (parse_string "*" $> TIMES) in
      let* operands = some (descend_into_or_lift @@ parse_arith_ ()) in
      pure
      @@
      match op with
      | PLUS -> Polynomial.sum @@ Sequence.of_list operands
      | TIMES -> Polynomial.product @@ Sequence.of_list operands
      | MINUS -> (
          match operands with
          | [] -> Polynomial.zero
          | [ o ] -> Polynomial.neg o
          | o :: os -> Polynomial.(o - sum (Sequence.of_list os)))
    in

    parse_expr <|> parse_int <|> parse_var_poly
  in
  parse_arith_ ()


let find_and_add_free_names rename_map vs =
  let find_and_add_free_name rename_map v =
    let v' =
      OurInt.(range_from one)
      |> Sequence.map ~f:(fun i -> Var.of_string (Var.to_string v ^ OurInt.to_string i))
      |> Sequence.append (Sequence.singleton v)
      |> Sequence.filter ~f:(not % Map.mem rename_map)
      |> Sequence.hd_exn
    in
    Map.set ~key:v ~data:v' rename_map
  in
  List.fold_left vs ~init:rename_map ~f:find_and_add_free_name


let parse_formula non_quantified_vars : Formulas.Formula.t ParseSexpListMonad.t =
  let rec parse_formula_ var_rename =
    let parse_arith = parse_arith ~var_rename in
    let parse_qf =
      let open Formulas in
      let* op =
        parse_string ">" $> GT
        <|> (parse_string ">=" $> GE)
        <|> (parse_string "=" $> EQ)
        <|> (parse_string "<" $> LT)
        <|> (parse_string "<=" $> LE)
        <|> (parse_string "and" $> AND)
        <|> (parse_string "or" $> OR)
        <|> (parse_string "distinct" $> NEQ)
      in
      let mk_comparison cmp = cmp <$> descend_into_or_lift parse_arith <%> descend_into_or_lift parse_arith in
      match op with
      | GT -> mk_comparison Formula.mk_gt
      | GE -> mk_comparison Formula.mk_ge
      | EQ -> mk_comparison Formula.mk_eq
      | LT -> mk_comparison Formula.mk_lt
      | LE -> mk_comparison Formula.mk_le
      | NEQ -> mk_comparison Formula.mk_uneq
      | AND -> Formula.all <$> many (descend_into_or_lift (parse_formula_ var_rename))
      | OR -> Formula.any <$> many (descend_into_or_lift (parse_formula_ var_rename))
    in
    let parse_with_quantifiers =
      let* () = parse_string "exists" <|> parse_string "forall" %> error "forall not supported" in
      let* quant_vars = descend_into (some (descend_into (parse_var <% parse_int))) in
      let temp_var_rename = find_and_add_free_names var_rename quant_vars in
      descend_into (parse_formula_ temp_var_rename)
    in
    parse_qf <|> parse_with_quantifiers
  in
  let var_rename =
    Set.to_sequence non_quantified_vars
    |> Sequence.map ~f:(fun v -> (v, v))
    |> Map.of_sequence_exn (module Var)
  in
  parse_formula_ var_rename


let parse_rule loc_arities : Transition.t List.t ParseSexpListMonad.t =
  descend_into_or_lift
  @@
  let get_arity loc : Int.t ParseSexpListMonad.t =
    match Map.find loc_arities loc with
    | Some ar -> pure ar
    | None -> error @@ "location " ^ Location.to_string loc ^ " in start point not defined"
  in
  let parse_rule_start =
    let* loc = parse_loc in
    let* ar = get_arity loc in
    let* vars = replicateM ar (Var.of_string <$> parse_atom) in
    pure (loc, vars)
  in
  let parse_rule_target =
    let* loc = parse_loc in
    let* ar = get_arity loc in
    let* updates = replicateM ar (PolyFunctionCall.of_poly <$> descend_into_or_lift parse_arith) in
    pure (loc, updates)
  in
  let* () = parse_string "rule" in
  let* start_loc, patterns = descend_into_or_lift parse_rule_start in
  let* target_loc, updates = descend_into_or_lift parse_rule_target in
  let non_quantified_vars =
    Sequence.of_list updates |> Sequence.map ~f:UpdateElement.vars
    |> Sequence.append (Sequence.singleton (VarSet.of_list patterns))
    |> Sequence.fold ~init:VarSet.empty ~f:Set.union
  in
  let* guard =
    parse_opt (parse_string ":guard" %> descend_into_or_lift (parse_formula non_quantified_vars))
    |> map (Option.value ~default:Formulas.Formula.mk_true)
  in
  let cost = Polynomials.Polynomial.one in
  let* next_sexps = get_sexps in
  let transitions =
    (* get rid of disjunctions *)
    Formulas.Formula.constraints guard
    |> List.map ~f:(fun constr ->
           ( start_loc,
             TransitionLabel.mk ~id:None ~cost ~patterns ~guard:constr ~assignments:updates,
             target_loc ))
  in
  pure transitions


module LocationMap = MakeMapCreators1 (Location)

let sexp_list_parser : Program.t ParseSexpListMonad.t =
  let* () = parse_format in
  let* () = parse_theory in
  let* location_arities_map = map LocationMap.of_alist_exn (some parse_loc_and_arity) in
  let* entryloc = parse_entrypoint in
  let* rules = List.concat <$> many (parse_rule location_arities_map) in
  let* () = parse_eos in
  pure @@ Program.from_sequence entryloc (Sequence.of_list rules)


let from_file filename =
  let sexps = parse_sexps_from_file filename in
  run_parser sexp_list_parser sexps


exception AriParserExn of string

let from_file_exn filename =
  match from_file filename with
  | Ok p -> p
  | Error e -> raise (AriParserExn e)
