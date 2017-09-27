open Batteries

(** Provides a unified interface of the parser and lexer for transition graphs.
    With this module it is possible to abstract from the details of parsing and lexing *)

(** Constructs a reader for the given transition graph *)
(*module Make(G : Parseable.Program) :
  sig*)
    module Parser_ = Parser
    module Lexer_  = Lexer

    exception Error of string
                                 
    val read_file : string -> Program.t

    val read_transitiongraph : string -> Program.t

    val read_formula : string -> Program.Formula_.t

    val read_constraint : string -> Program.Constraint_.t
      
    val read_atom : string -> Program.Atom_.t

    val read_polynomial : string -> Program.Polynomial_.t

    val read_bound : string -> Program.TransitionLabel.Bound.t
      
(*  end*)
