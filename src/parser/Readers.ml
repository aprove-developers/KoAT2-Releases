open Batteries
open ConstraintTypes
open Lexing

let position_string lexbuf =
  let pos = lexbuf.lex_curr_p in
  Printf.sprintf "line %d at char number %d which is directly after %s"
                 pos.pos_lnum
                 (pos.pos_cnum - pos.pos_bol + 1)
                 (lexeme lexbuf)

let read_ rule lexbuf =
  ParserUtil.empty_cache ();
  try rule Lexer.read lexbuf with
  | Lexer.SyntaxError msg ->
     raise (ParserUtil.Error (Printf.sprintf "%s at %s" msg (position_string lexbuf)))
  | Parser.Error ->
     raise (ParserUtil.Error (Printf.sprintf "Parser error at %s" (position_string lexbuf)))

let read_file path =
  read_ Parser.onlyProgram (Lexing.from_channel @@ File.open_in path)

let read rule str =
  read_ rule (Lexing.from_string str)

let read_program =
  read Parser.onlyProgram

let read_program_simple =
  read Parser.onlyProgram_simple

let read_formula =
  read Parser.onlyFormula

let read_general_transitions =
  read Parser.general_transitions

let read_constraint =
  read Parser.onlyConstraints

let read_probability_distribution =
  read Parser.onlyProbabilityDistribution

let read_update_element =
  read Parser.onlyUpdateElement

let read_atom =
  read Parser.onlyAtom

let read_polynomial =
  read Parser.onlyPolynomial

let read_bound =
  read Parser.onlyBound

let read_input ?(rename=false) simple program_str =
  if simple then
    program_str
    |> read_program_simple
    |> (if rename then Program_.rename else identity)
  else
    try
      program_str
      |> read_file
      |> (if rename then Program_.rename else identity)
    with Program_.RecursionNotSupported ->
      failwith "ERROR: The given program uses recursion. Recursion is not supported by the current version of koat2. The program will exit now."

let read_prog_goal_file ?(rename=false) path =
  read_ Parser.programAndGoal (Lexing.from_channel @@ File.open_in path)
  |> Tuple2.map1 (if rename then Program_.rename else identity)

let read_probabilistic_prog_goal_file path =
  read_ Parser.probabilisticProgramAndGoal (Lexing.from_channel @@ File.open_in path)

