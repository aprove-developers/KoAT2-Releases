
module Make
          (P : PolyTypes.ParseablePolynomial)
: sig
  
  (* The type of tokens. *)
  
  type token = 
    | UINT of (int)
    | TIMES
    | RPAR
    | POW
    | PLUS
    | MINUS
    | LPAR
    | ID of (string)
    | EOF
  
  (* This exception is raised by the monolithic API functions. *)
  
  exception Error
  
  (* The monolithic API. *)
  
  val polynomial: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (P.t)
  
end
