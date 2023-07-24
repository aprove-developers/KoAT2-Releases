{
  (** Is used to generate a lexer with ocamllex to lex transition graphs, its constraints and polynomials. *)

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
let comment = '#'[^ '\n' '\r']*newline
let id = ['a'-'z' 'A'-'Z' '_' '!'] ['a'-'z' 'A'-'Z' '0'-'9' '.' '_' '\'' '!']* '\''?

let fraction = ['-']?['0'-'9']+['/']['1'-'9']['0'-'9']*
let probfloat = ['+']?(['0'-'1']*)?['.']['0'-'9']+ | '1''.''0'* | '[' fraction ']'

rule read =
  parse
  | white             { read lexbuf }
  | newline           { next_line lexbuf; read lexbuf }
  | comment           { next_line lexbuf; read lexbuf }
  | "GOAL"            { P.GOAL }
  | "COMPLEXITY"           { P.COMPLEXITY }
  | "EXPECTEDCOMPLEXITY"   { P.EXPECTEDCOMPLEXITY }
  (*| "EXACTRUNTIME"         { P.EXACTRUNTIME }
  | "EXPECTEDSIZE"    { P.EXPECTEDSIZE } *)
  | "BERNOULLI"       { P.BERNOULLI }
  | "BINOMIAL"        { P.BINOMIAL }
  | "GEOMETRIC"       { P.GEOMETRIC }
  | "HYPERGEOMETRIC"  { P.HYPERGEOMETRIC }
  | "UNIFORM"         { P.UNIFORM }
  | "STARTTERM"       { P.STARTTERM }
  | "FUNCTIONSYMBOLS" { P.FUNCTIONSYMBOLS }
  | "RULES"           { P.RULES }
  | "VAR"             { P.VAR }
  | "inf"             { P.INFINITY }
  | int               { P.UINT (Lexing.lexeme lexbuf) }
  | probfloat         { P.UFLOAT (Lexing.lexeme lexbuf)}
  | id                { P.ID (Lexing.lexeme lexbuf) }
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
  | "**"              { P.POW }
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
  | ":+:"             { P.PROBDIV }
  | ":"               { P.COLON }
  | ','               { P.COMMA }
  | eof               { P.EOF }
  | _                 { raise (SyntaxError ("Unexpected char: " ^ Lexing.lexeme lexbuf)) }

(*{
  end
}*)
