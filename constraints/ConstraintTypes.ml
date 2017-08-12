open Batteries
open PolyTypes

module type Atom =
  sig
        module Polynomial_ : Polynomial

        module Comparator : sig
          type t = GT | GE | LT | LE | NEQ | EQ
          val values : t list
          val to_string : t -> string
        end
                                                  
        type t = Polynomial_.t * Comparator.t * Polynomial_.t
             
        val comparator : t -> Comparator.t

        (*getting information*)
        val fst : t -> Polynomial_.t
        val snd : t -> Polynomial_.t
        
        (*creation*)
        val mk : Comparator.t -> Polynomial_.t -> Polynomial_.t -> t
        val mk_gt : Polynomial_.t -> Polynomial_.t -> t
        val mk_ge : Polynomial_.t -> Polynomial_.t -> t
        val mk_lt : Polynomial_.t -> Polynomial_.t -> t
        val mk_le : Polynomial_.t -> Polynomial_.t -> t
        val mk_eq : Polynomial_.t -> Polynomial_.t -> t
        val mk_neq : Polynomial_.t -> Polynomial_.t -> t
        
        (*boolean tests*)
        val is_gt : t -> bool
        val is_ge : t -> bool
        val is_lt : t -> bool
        val is_le : t -> bool
        val is_eq : t -> bool
        val is_neq : t -> bool
        val is_same : t -> t -> bool
        val is_inverted : t -> t -> bool
        val is_redundant : t -> t -> bool
        val (==) : t -> t -> bool
        
        (*export*)
        val to_string : t -> string
        val vars : t -> Polynomial_.Var.t list
        val rename : t -> Polynomial_.RenameMap_.t -> t
        val eval_bool : t -> Polynomial_.Valuation_.t -> bool
    end

module type Constraint =
  sig
        module Atom_ : Atom

        type t = Atom_.t list  
        
        (*getting information*)
        val vars : t -> Atom_.Polynomial_.Var.t list
        
        (*creation*)
        val lift : Atom_.t -> t      
        val mk : Atom_.t list -> t
        val mk_true : t
        
        (*boolean tests*)
        val is_true : t -> bool

        (*export*)
        val to_string : t -> string
        
        val rename : t -> Atom_.Polynomial_.RenameMap_.t -> t
        val eval_bool : t -> Atom_.Polynomial_.Valuation_.t -> bool
    end
