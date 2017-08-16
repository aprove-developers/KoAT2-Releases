open Batteries

(** Provides module types which represent a minimal subset of their complete counterparts in the modules contraints and poly *)
(** Those are the minimal methods needed to construct the types from a textual representation *)

module type Polynomial =
  sig
    type t
    module Var : PolyTypes.ID
    val from_constant_int : int -> t
    val from_var_string : string -> t
    val to_string : t -> string
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
    val mk_eq : Polynomial_.t -> Polynomial_.t -> t
    val mk_neq : Polynomial_.t -> Polynomial_.t -> t
    val to_string : t -> string
  end
  
module type Constraint =
  sig
    type t
    module Atom_ : Atom
    
    val to_string : t -> string
    val mk : Atom_.t list -> t
    val is_true : t -> bool
  end
   
module type Location =
  sig
    type t
    val to_string : t -> string
    val of_string : string -> t       
  end
   
module type Transition =
  sig
    type t
    module Constraint_ : Constraint
    val mk : string ->
             Constraint_.Atom_.Polynomial_.Var.t list ->
             Constraint_.Atom_.Polynomial_.t list ->
             Constraint_.t ->
             Constraint_.Atom_.Polynomial_.Var.t list ->
             t
    val to_string : string -> string -> t -> string
  end

module type TransitionGraph =
  sig
    type t
    module Transition_ : Transition
    module Location_ : Location
    val from : Transition_.Constraint_.Atom_.Polynomial_.Var.t list
               -> (string * string * Transition_.t) list
               -> t
    val to_string : t -> string
  end

