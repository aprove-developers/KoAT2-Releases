{
  (** Provides an lexer to parse the output of the sage script *)
  
  (** Constructs a lexer for transition graphs as well as its used constraints and polynomials *)
(*  module Make(G : Parseable.Program) =
    struct*)
      open Lexing
      module P = ExactParser(*.Make(G)*)
         
      exception SyntaxError of string
                             
      let next_line lexbuf =
        let pos = lexbuf.lex_curr_p in
        lexbuf.lex_curr_p <-
          { pos with pos_bol = lexbuf.lex_curr_pos;
                     pos_lnum = pos.pos_lnum + 1
          }

}

let float = (['0'-'9']*)?['.']['0'-'9']+
let variable = 'x'['0'-'9']*
let fraction = ['0'-'9']+['/']['1'-'9']['0'-'9']*
let white = [' ' '\t']+

rule read =
  parse
  | white             { read lexbuf }
  | fraction          { P.FRACTION (Lexing.lexeme lexbuf) }
  | variable          { P.VAR (Lexing.lexeme lexbuf) }
  | float             { P.FLOAT (Lexing.lexeme lexbuf) }
  | '('               { P.LPAR }
  | ')'               { P.RPAR }
  | '['               { P.LBRK }
  | ']'               { P.RBRK }
  | '+'               { P.PLUS }
  | '*'               { P.TIMES }
  | '-'               { P.MINUS }
  | '^'               { P.POW }
  | "cos"             { P.COS }
  | "sin"             { P.SIN }
  | ','               { P.COMMA }
  | eof               { P.EOF }

(*{
  end
}*)


