open! OurBase
open Lexing

let position_string lexbuf =
  let pos = lexbuf.lex_curr_p in
  Printf.sprintf "line %d at char number %d which is directly after %s" pos.pos_lnum
    (pos.pos_cnum - pos.pos_bol + 1)
    (lexeme lexbuf)


let read_ rule (lexbuf : Lexing.lexbuf) =
  KoatParserUtil.empty_cache ();
  try rule Koat_lexer.read lexbuf with
  | Koat_lexer.SyntaxError msg ->
      raise (KoatParserUtil.Error (Printf.sprintf "%s at %s" msg (position_string lexbuf)))
  | Koat_parser.Error ->
      raise (KoatParserUtil.Error (Printf.sprintf "KoatParser error at %s" (position_string lexbuf)))


let read_target_from_file_ target path =
  read_ target (Lexing.from_channel @@ Stdio.In_channel.create ~binary:false path)


let read_file ?(termination = false) =
  if termination then
    read_target_from_file_ Koat_parser.onlyProgramTermination
  else
    read_target_from_file_ Koat_parser.onlyProgram


let read rule str = read_ rule (Lexing.from_string str)
let read_program = read Koat_parser.onlyProgram
let read_program_simple = read Koat_parser.onlyProgram_simple
let read_formula = read Koat_parser.onlyFormula
let read_general_transitions = read Koat_parser.general_transitions
let read_constraint = read Koat_parser.onlyConstraints
let read_probability_distribution = read Koat_parser.onlyProbabilityDistribution
let read_update_element = read Koat_parser.onlyUpdateElement
let read_atom = read Koat_parser.onlyAtom
let read_polynomial str = read Koat_parser.onlyPolynomial str
let read_polynomialRec str = read Koat_parser.onlyPolynomialRec str
let read_bound = read Koat_parser.onlyBound
let read_rational_bound = read Koat_parser.onlyRationalBound
let read_prob str = read Koat_parser.onlyProb str

let read_input ?(termination = false) ?(rename = false) simple program_str =
  if simple then
    program_str |> read_program_simple
    |>
    if rename then
      Program_.rename
    else
      identity
  else
    try
      program_str |> read_file ~termination
      |>
      if rename then
        Program_.rename
      else
        identity
    with
    | Program_.RecursionNotSupported ->
        failwith
          "ERROR: The given program uses recursion. Recursion is not supported by the current version of \
           koat2. The program will exit now."


let read_prog_goal_file ?(rename = false) path =
  read_target_from_file_ Koat_parser.programAndGoal path
  |> Tuple2.map1
       (if rename then
          Program_.rename
        else
          identity)


let read_probabilistic_program = read_target_from_file_ Koat_parser.onlyProbabilisticProgram
let read_probabilistic_prog_goal_file = read_target_from_file_ Koat_parser.probabilisticProgramAndGoal
