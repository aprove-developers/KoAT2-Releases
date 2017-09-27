open Batteries

(** Provides module types which represent a minimal subset of their complete counterparts in the modules contraints and poly.
    Those are the minimal methods needed to construct the types from a textual representation *)

module type Polynomial =
  sig
    type t
    module Value : PolyTypes.Ring
    val value : int -> t
    val var : string -> t
    include PolyTypes.BaseMath with type t := t
  end

module type MinMaxPolynomial =
  sig
    type t
    module Polynomial_ : Polynomial
    val of_poly : Polynomial_.t -> t              
    val minimum : t list -> t
    val maximum : t list -> t
    val sum : t list -> t
    val product : t list -> t
    val infinity : t
    val neg : t -> t
    val exp : Polynomial_.Value.t -> t -> t
  end
  
module type Atom =
  sig
    type t
    module Polynomial_ : Polynomial
    val mk_gt : Polynomial_.t -> Polynomial_.t -> t
    val mk_ge : Polynomial_.t -> Polynomial_.t -> t
    val mk_lt : Polynomial_.t -> Polynomial_.t -> t
    val mk_le : Polynomial_.t -> Polynomial_.t -> t
  end
  
module type Constraint =
  sig
    type t
    module Polynomial_ : Polynomial
    module Atom_ : Atom with module Polynomial_ = Polynomial_
    
    val mk : Atom_.t list -> t
    val mk_eq : Polynomial_.t -> Polynomial_.t -> t
    val mk_gt : Polynomial_.t -> Polynomial_.t -> t
    val mk_ge : Polynomial_.t -> Polynomial_.t -> t
    val mk_lt : Polynomial_.t -> Polynomial_.t -> t
    val mk_le : Polynomial_.t -> Polynomial_.t -> t
    val all : t list -> t
    val is_true : t -> bool
  end

module type Formula =
  sig
    type t
    module Polynomial_ : Polynomial
    module Atom_ : Atom with module Polynomial_ = Polynomial_
    module Constraint_ : Constraint with module Polynomial_ = Polynomial_
    
    val lift : Atom_.t -> t      
    val all : t list -> t
    val disj : Constraint_.t list -> t
  end
  
module type Monadize =
    sig
        module Outer : Polynomial
        module Inner : Polynomial
        val flatten : Outer.t -> Inner.t
    end
  
(*module type Program =
  sig
    type t
    module PolynomialMonad_ : Monadize
    module Polynomial_ = PolynomialMonad_.Inner
    module Atom_ : Atom with module Polynomial_ = PolynomialMonad_.Inner
    module Constraint_ : Constraint with module Polynomial_ = PolynomialMonad_.Inner and module Atom_ = Atom_
    module Formula_ : Formula with module Polynomial_ = PolynomialMonad_.Inner and module Constraint_ = Constraint_ and module Atom_ = Atom_ 
    module TransitionLabel :
    sig
      type t
      module Bound : (MinMaxPolynomial with module Polynomial_ = PolynomialMonad_.Inner)
      exception RecursionNotSupported
      val mk : name:string ->
               start:string ->
               targets:(string * (Constraint_.Polynomial_.t list)) list ->
               patterns:Var.t list ->
               guard:Constraint_.t ->
               vars:Var.t list ->
               t
      val start : t -> string
      val target : t -> string
    end
    module Location :
      sig
        type t
        val of_string : string -> t       
      end
   
    val from : Var.t list
               -> TransitionLabel.t list
               -> Location.t
               -> t
  end*)

