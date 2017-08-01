open Batteries
open ConstraintTypes
open Lexing
   
module MakeReader(G : Parseable.TransitionGraph) =
  struct
    module Parser = Parser.Make(G)
    module Lexer = Lexer.Make(G)
                 
    let print_position outx lexbuf =
      let pos = lexbuf.lex_curr_p in
      Printf.fprintf outx "%s:%d:%d" pos.pos_fname
              pos.pos_lnum (pos.pos_cnum - pos.pos_bol + 1)

    (* Takes a string and tries to parse and lexe it to the wanted type *)
    let read rule str =
      let lexbuf = Lexing.from_string str in
      try rule Lexer.read lexbuf with
      | Lexer.SyntaxError msg ->
         Printf.fprintf stderr "%a: %s\n" print_position lexbuf msg;
         raise (Lexer.SyntaxError msg)
      | Parser.Error ->
         Printf.fprintf stderr "%a: syntax error\n" print_position lexbuf;
         raise Parser.Error

    let read_transitiongraph =
      read Parser.onlyTransitiongraph

    let read_constraint =
      read Parser.onlyConstraints
      
    let read_atom =
      read Parser.onlyAtom

    let read_polynomial =
      read Parser.onlyPolynomial

  end
