open Batteries

(** Provides a unified interface of the parser and lexer for transition graphs.
    With this module it is possible to abstract from the details of parsing and lexing *)

(** Constructs a reader for the given transition graph *)
module Make(G : Parseable.TransitionGraph) :
  sig
    module Parser_ : module type of Parser.Make(G)
    module Lexer_ : module type of Lexer.Make(G)

    exception Error of string
                                 
    val read_file : string -> G.t

    val read_transitiongraph : string -> G.t

    val read_constraint : string -> G.Transition_.Constraint_.t
      
    val read_atom : string -> G.Transition_.Constraint_.Atom_.t

    val read_polynomial : string -> G.Transition_.Constraint_.Atom_.Polynomial_.t

  end
