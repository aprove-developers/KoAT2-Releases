open Batteries
open BoundsInst
open ConstraintTypes
open Lexing
open ExactProgramTypes

exception Error of string

let position_string lexbuf =
  let pos = lexbuf.lex_curr_p in
  Printf.sprintf "line %d at char number %d which is directly after %s"
                 pos.pos_lnum
                 (pos.pos_cnum - pos.pos_bol + 1)
                 (lexeme lexbuf)

let read_ rule lexbuf =
  try rule Lexer.read lexbuf with
  | Lexer.SyntaxError msg ->
     raise (Error (Printf.sprintf "%s at %s" msg (position_string lexbuf)))
  | Parser.Error ->
     raise (Error (Printf.sprintf "Parser error at %s" (position_string lexbuf)))

let read_file trans_id_counter path =
  read_ Parser.onlyProgram (Lexing.from_channel @@ File.open_in path) trans_id_counter

let read rule str =
  read_ rule (Lexing.from_string str)

let read_program trans_id_counter str =
  (read Parser.onlyProgram) str trans_id_counter

let read_program_simple =
  flip (read Parser.onlyProgram_simple)

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

let read_goal_file path =
  read_ Parser.onlyGoal (Lexing.from_channel @@ File.open_in path)

let read_prog_goal_file trans_id_counter path =
  read_ Parser.programAndGoal (Lexing.from_channel @@ File.open_in path) @@ trans_id_counter

let read_exact_file path =
  read_ Parser.exactProgram (Lexing.from_channel @@ File.open_in path)
