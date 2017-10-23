open Batteries
open ConstraintTypes
open Lexing

exception Error of string
                 
let position_string lexbuf =
  let pos = lexbuf.lex_curr_p in
  Printf.sprintf "%s:%d:%d"
                 pos.pos_fname
                 pos.pos_lnum
                 (pos.pos_cnum - pos.pos_bol + 1)

let read_ rule lexbuf =
  try rule Lexer.read lexbuf with
  | Lexer.SyntaxError msg ->
     raise (Error (Printf.sprintf "%s: %s" msg (position_string lexbuf)))
  | Parser.Error ->
     raise (Error (Printf.sprintf "%s: %s" "ParserError" (position_string lexbuf)))
    
let read_file path =
  read_ Parser.onlyTransitiongraph (Lexing.from_input (File.open_in path))

let read rule str =
  read_ rule (Lexing.from_string str)

let read_transitiongraph =
  read Parser.onlyTransitiongraph

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
