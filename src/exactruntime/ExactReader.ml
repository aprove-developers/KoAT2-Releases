open Lexing

exception Error of string

let position_string lexbuf =
  let pos = lexbuf.lex_curr_p in
  Printf.sprintf "line %d at char number %d which is directly after %s"
                 pos.pos_lnum
                 (pos.pos_cnum - pos.pos_bol + 1)
                 (lexeme lexbuf)

let read_ rule lexbuf =
  try rule ExactLexer.read lexbuf with
  | ExactLexer.SyntaxError msg ->
     raise (Error (Printf.sprintf "%s at %s" msg (position_string lexbuf)))
  | ExactParser.Error ->
     raise (Error (Printf.sprintf "Parser error at %s" (position_string lexbuf)))

let read rule str =
  read_ rule (Lexing.from_string str)

let from_tree =
  read ExactParser.from_tree
