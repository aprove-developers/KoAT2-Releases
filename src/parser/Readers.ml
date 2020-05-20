open Batteries
open ConstraintTypes
open Lexing

exception Error of string
                 
let position_string lexbuf =
  let pos = lexbuf.lex_curr_p in
  Printf.sprintf "line %d at char number %d which is directly after %s"
                 pos.pos_lnum
                 (pos.pos_cnum - pos.pos_bol + 1)
                 (lexeme lexbuf)

let read_ rule lexbuf =
  let result =
    try rule Lexer.read lexbuf with
    | Lexer.SyntaxError msg ->
      raise (Error (Printf.sprintf "%s at %s" msg (position_string lexbuf)))
    | Parser.Error ->
      raise (Error (Printf.sprintf "Parser error at %s" (position_string lexbuf)))
  in
  ParserUtil.clear_stored_arities;
  result
    
let read_file path =
  read_ Parser.onlyProgram (Lexing.from_input (File.open_in path))

let read rule str =
  read_ rule (Lexing.from_string str)

let read_program =
  read Parser.onlyProgram

let read_program_simple =
  read Parser.onlyProgram_simple

let read_formula =
  read Parser.onlyFormula

let read_constraint =
  read Parser.onlyConstraints
  
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
    |> (if rename then Program.rename else identity)
    |> Option.some
  else
    try
      program_str
      |> read_file
      |> (if rename then Program.rename else identity)
      |> Option.some
    with TransitionLabel.RecursionNotSupported ->
      prerr_string "ERROR: The given program uses recursion. Recursion is not supported by the current version of koat2. The program will exit now."; None
