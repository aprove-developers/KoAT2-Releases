open Batteries
open ConstraintTypes
open Lexing

(*module Make(G : Parseable.Program) =
  struct*)
    module Parser_ = Parser
    module Lexer_ = Lexer
    exception Error of string
                                 
    let position_string lexbuf =
      let pos = lexbuf.lex_curr_p in
      Printf.sprintf "%s:%d:%d"
                     pos.pos_fname
                     pos.pos_lnum
                     (pos.pos_cnum - pos.pos_bol + 1)

    let read_ rule lexbuf =
      try rule Lexer_.read lexbuf with
      | Lexer_.SyntaxError msg ->
         raise (Error (Printf.sprintf "%s: %s" msg (position_string lexbuf)))
      | Parser_.Error ->
         raise (Error (Printf.sprintf "%s: %s" "ParserError" (position_string lexbuf)))
      
    let read_file path =
      read_ Parser_.onlyTransitiongraph (Lexing.from_input (File.open_in path))

    let read rule str =
      read_ rule (Lexing.from_string str)

    let read_transitiongraph =
      read Parser_.onlyTransitiongraph

    let read_formula =
      read Parser_.onlyFormula

    let read_constraint =
      read Parser_.onlyConstraints
      
    let read_atom =
      read Parser_.onlyAtom

    let read_polynomial =
      read Parser_.onlyPolynomial

    let read_bound =
      read Parser_.onlyBound

 (* end*)
