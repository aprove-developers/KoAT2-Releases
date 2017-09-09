open Batteries

(** Provides module types which represent a minimal subset of their complete counterparts in the modules contraints and poly.
    Those are the minimal methods needed to construct the types from a textual representation *)

module type Polynomial =
  sig
    type t
    module Var : PolyTypes.ID
    val value : int -> t
    val var : string -> t
    include PolyTypes.BaseMath with type t := t
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
   
module type Program =
  sig
    type t
    module Constraint_ : Constraint
    module TransitionLabel :
    sig
      type t
      exception RecursionNotSupported
      val mk : name:string ->
               start:string ->
               targets:(string * (Constraint_.Polynomial_.t list)) list ->
               patterns:Constraint_.Polynomial_.Var.t list ->
               guard:Constraint_.t ->
               vars:Constraint_.Polynomial_.Var.t list ->
               t
      val start : t -> string
      val target : t -> string
    end
    module Location :
      sig
        type t
        val of_string : string -> t       
      end
   
    val from : Constraint_.Polynomial_.Var.t list
               -> TransitionLabel.t list
               -> Location.t
               -> t
  end

