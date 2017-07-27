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
  
module type PolynomialConstraintsAtom =
  sig
        module Var : ID
        module Value : Number.Numeric
        module Polynomial_ : (Polynomial with module Var = Var and module Value = Value)
             
        type t = 
          |GreaterThan of Polynomial_.t * Polynomial_.t
          |GreaterEqual of Polynomial_.t * Polynomial_.t
          |LessThan of Polynomial_.t * Polynomial_.t
          |LessEqual of Polynomial_.t * Polynomial_.t
          |Neq of Polynomial_.t * Polynomial_.t
          |Equal of Polynomial_.t * Polynomial_.t

        (* TODO Make t = polynomial * comparator * polynomial for comfortability *)
        type comparator = GT | GE | LT | LE | NEQ | EQ        

        val get_comparator : t -> comparator

        (*getting information*)
        val get_first_arg : t -> Polynomial_.t
        val get_second_arg : t -> Polynomial_.t
        
        (*creation*)
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
        val get_variables : t -> Var.t list
        val rename_vars : t -> Polynomial_.RenameMap_.t -> t
        val eval_bool : t -> Polynomial_.Valuation_.t -> bool
    end

module type PolynomialConstraints =
  sig
        module Var : ID
        module Value : Number.Numeric
        module Polynomial_ : (Polynomial with module Var = Var and module Value = Value)
        module PolynomialConstraintsAtoms_ : (PolynomialConstraintsAtom with module Var = Var and module Value = Value and module Polynomial_ = Polynomial_)

        type atom = PolynomialConstraintsAtoms_.t
        type t = atom list  
        
        (*getting information*)
        val get_variables : t -> Var.t list
        
        (*creation*)
        val lift : atom -> t      
        val mk : atom list -> t
        
        (*boolean tests*)

        (*export*)
        val to_string : t -> string
        
        val rename_vars : t -> Polynomial_.RenameMap_.t -> t
        val eval_bool : t -> Polynomial_.Valuation_.t -> bool
    end
    
module type ParseablePolynomialConstraints =
  sig
    type t
    module Polynomial_ : ParseablePolynomial
    module PolynomialConstraintsAtoms_ : ParseablePolynomialConstraintsAtom
    
    type atom = PolynomialConstraintsAtoms_.t
    val to_string : t -> string
    val mk : atom list -> t
  end
