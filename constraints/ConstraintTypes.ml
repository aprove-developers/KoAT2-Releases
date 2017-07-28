open Batteries
open PolyTypes

module type ParseablePolynomialConstraintsAtom =
  sig
    type t
    module Polynomial_ : ParseablePolynomial
    val mk_gt : Polynomial_.t -> Polynomial_.t -> t
    val mk_ge : Polynomial_.t -> Polynomial_.t -> t
    val mk_lt : Polynomial_.t -> Polynomial_.t -> t
    val mk_le : Polynomial_.t -> Polynomial_.t -> t
    val mk_eq : Polynomial_.t -> Polynomial_.t -> t
    val mk_neq : Polynomial_.t -> Polynomial_.t -> t
    val to_string : t -> string
  end
  
module type ParseablePolynomialConstraints =
  sig
    type t
    module PolynomialConstraintsAtoms_ : ParseablePolynomialConstraintsAtom
    
    val to_string : t -> string
    val mk : PolynomialConstraintsAtoms_.t list -> t
  end
   
module type PolynomialConstraintsAtom =
  sig
        module Polynomial_ : Polynomial

        type comparator = GT | GE | LT | LE | NEQ | EQ        

        type t = Polynomial_.t * comparator * Polynomial_.t
             
        val get_comparator : t -> comparator

        (*getting information*)
        val get_first_arg : t -> Polynomial_.t
        val get_second_arg : t -> Polynomial_.t
        
        (*creation*)
        val mk : comparator -> Polynomial_.t -> Polynomial_.t -> t
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
        val is_same_constr : t -> t -> bool
        val is_inverted_constr : t -> t -> bool
        val is_redundant : t -> t -> bool
        val (==) : t -> t -> bool
        
        (*export*)
        val to_string : t -> string
        val get_variables : t -> Polynomial_.Var.t list
        val rename_vars : t -> Polynomial_.RenameMap_.t -> t
        val eval_bool : t -> Polynomial_.Valuation_.t -> bool
    end

module type PolynomialConstraints =
  sig
        module PolynomialConstraintsAtoms_ : PolynomialConstraintsAtom

        type t = PolynomialConstraintsAtoms_.t list  
        
        (*getting information*)
        val get_variables : t -> PolynomialConstraintsAtoms_.Polynomial_.Var.t list
        
        (*creation*)
        val lift : PolynomialConstraintsAtoms_.t -> t      
        val mk : PolynomialConstraintsAtoms_.t list -> t
        
        (*boolean tests*)

        (*export*)
        val to_string : t -> string
        
        val rename_vars : t -> PolynomialConstraintsAtoms_.Polynomial_.RenameMap_.t -> t
        val eval_bool : t -> PolynomialConstraintsAtoms_.Polynomial_.Valuation_.t -> bool
    end
