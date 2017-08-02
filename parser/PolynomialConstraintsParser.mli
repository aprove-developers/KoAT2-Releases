
module Make
          (C : ConstraintTypes.ParseablePolynomialConstraints)
: sig
  
  (* The type of tokens. *)
  
  type token = 
    | UINT of (int)
    | TIMES
    | RPAR
    | POW
    | PLUS
    | NEQ
    | MINUS
    | LPAR
    | LESSTHAN
    | LESSEQUAL
    | ID of (string)
    | GREATERTHAN
    | GREATEREQUAL
    | EQUAL
    | EOF
    | COMMA
    | AND
  
  (* This exception is raised by the monolithic API functions. *)
  
  exception Error
  
  (* The monolithic API. *)
  
  val polynomialConstraints: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (C.t)
  
  val polynomialConstraintAtom: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (C.atom)
  
  val polynomial: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (C.PolynomialConstraintsAtoms_.Polynomial_.t)
  
end
