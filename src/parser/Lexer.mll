{
  (** Provides an lexer generated with ocamllex to lex transition graphs, its constraints and polynomials. *)
  
  (** Constructs a lexer for transition graphs as well as its used constraints and polynomials *)
(*  module Make(G : Parseable.Program) =
    struct*)
      open Lexing
      module P = Parser(*.Make(G)*)
         
      exception SyntaxError of string
                             
      let next_line lexbuf =
        let pos = lexbuf.lex_curr_p in
        lexbuf.lex_curr_p <-
          { pos with pos_bol = lexbuf.lex_curr_pos;
                     pos_lnum = pos.pos_lnum + 1
          }

}

let int = ['0'-'9'] ['0'-'9']*
let white = [' ' '\t']+
let newline = '\r' | '\n' | "\r\n"
let id = ['a'-'z' 'A'-'Z' '_'] ['a'-'z' 'A'-'Z' '0'-'9' '_']* '\''?

rule read =
  parse
  | white             { read lexbuf }
  | newline           { next_line lexbuf; read lexbuf }
  | "GOAL"            { P.GOAL }
  | "STARTTERM"       { P.STARTTERM }
  | "FUNCTIONSYMBOLS" { P.FUNCTIONSYMBOLS }
  | "RULES"           { P.RULES }
  | "VAR"             { P.VAR }
  | "min"             { P.MIN }
  | "max"             { P.MAX }
  | "inf"             { P.INFINITY }
  | int               { P.UINT (int_of_string (Lexing.lexeme lexbuf)) }
  | id                { P.ID (Lexing.lexeme lexbuf) }
  | '|'               { P.ABS }
  | '('               { P.LPAR }
  | ')'               { P.RPAR }
  | '{'               { P.LBRACE }
  | '}'               { P.RBRACE }
  | '['               { P.LBRACK }
  | ']'               { P.RBRACK }
  | '+'               { P.PLUS }
  | '*'               { P.TIMES }
  | '-'               { P.MINUS }
  | '^'               { P.POW }
  | "->"              { P.ARROW }
  | "="               { P.EQUAL }
  | "!="              { P.UNEQUAL }
  | "<="              { P.LESSEQUAL }
  | ">="              { P.GREATEREQUAL }
  | '<'               { P.LESSTHAN }
  | '>'               { P.GREATERTHAN }
  | "&&"              { P.AND }
  | "/\\"             { P.AND }
  | "||"              { P.OR }
  | ":|:"             { P.WITH }
  | ','               { P.COMMA }
  | eof               { P.EOF }
  | _                 { raise (SyntaxError ("Unexpected char: " ^ Lexing.lexeme lexbuf)) }

(*{
  end
}*)
