open Batteries
open PolyTypes

(** Provides all module types related to constraints *)

(** An atom is a comparison between two polynomials *)
module type Atom =
  sig
        module Polynomial_ : Polynomial

        (** A comparator sets two values in a binary relation *)
        module Comparator : sig

          (** The different comparators.
              GT -> a > b.
              GE -> a >= b.
              LT -> a < b.
              LE -> a <= b.
              NEQ -> a <> b.
              EQ -> a = b. *)
          type t = GT | GE | LT | LE | NEQ | EQ

          (** Returns a list of all possible comparators *)
          val values : t list

          val to_string : t -> string

        end
                                                  
        type t = Polynomial_.t * Comparator.t * Polynomial_.t
             
        (** Returns the comparator of the atom as-is. *)
        val comparator : t -> Comparator.t

        (** Returns the the left hand side of the constraint. *)
        val fst : t -> Polynomial_.t

        (** Returns the the right hand side of the constraint. *)
        val snd : t -> Polynomial_.t
        

        (** Following methods are convenience methods for the creation of atoms. *)
          
        val mk : Comparator.t -> Polynomial_.t -> Polynomial_.t -> t
        val mk_gt : Polynomial_.t -> Polynomial_.t -> t
        val mk_ge : Polynomial_.t -> Polynomial_.t -> t
        val mk_lt : Polynomial_.t -> Polynomial_.t -> t
        val mk_le : Polynomial_.t -> Polynomial_.t -> t
        val mk_eq : Polynomial_.t -> Polynomial_.t -> t
        val mk_neq : Polynomial_.t -> Polynomial_.t -> t
        

        (** Following methods manipulate atoms and return the manipulated versions. *)

        (** Transforms the atom to a form, where all variables occur on the lhs and the rhs consists of a single constant. 
            Example: (2*a^2 - 4b + 1 >= 2b - 7) gets transformed to (2*a^2 - 6b >= -8). *)
        val normalise : t->t

        (** Returns an atom which does not use strict comparators (GT (>=), LT (<=)). 
            To be correct, the underlying polynomial must be based on values which have unique predecessors and successors. 
            !! This is not checked at the moment !! *)
        val remove_strictness : t->t
        

        (** Following methods return if the atom has certain properties. *)
          
        val is_gt : t -> bool
        val is_ge : t -> bool
        val is_lt : t -> bool
        val is_le : t -> bool
        val is_eq : t -> bool
        val is_neq : t -> bool

        val is_same : t -> t -> bool

        (** Returns if both polynomials are linear. *)
        val is_linear : t -> bool

        (** Returns if the atoms are equivalent. *)
        val is_redundant : t -> t -> bool
          
        val (==) : t -> t -> bool
        
        val to_string : t -> string

        (** Returns the set of variables which are active in the atom.
            A variable is active, if it's value has an effect on the evaluation of the atom. *)
        val vars : t -> Polynomial_.Var.t Set.t
          
        (** Assigns the variables of the atom new names based on the rename map. *)
        val rename : t -> Polynomial_.RenameMap_.t -> t

        (** Assigns each variable a value and returns if the atom is satisfied for those values. *)
        val eval_bool : t -> Polynomial_.Valuation_.t -> bool
          
    end

(** A constraint is a conjunction of atoms *)
module type Constraint =
  sig
        module Atom_ : Atom

        type t = Atom_.t list (** TODO Should not be exposed *)
        

        (** Following methods are convenience methods for the creation of atoms. *)
          
        val lift : Atom_.t -> t      
        val mk : Atom_.t list -> t
        val mk_true : t
        

        (** Following methods return if the atom has certain properties. *)
          
        (** Returns if the constraint is a tautology *)
        val is_true : t -> bool

        (** Returns the set of variables which are active in the constraint.
            A variable is active, if it's value has an effect on the evaluation of the constraint. *)
        val vars : t -> Atom_.Polynomial_.Var.t Set.t

        val to_string : t -> string


        (** Following methods manipulate atoms and return the manipulated versions. *)

        (** Assigns the variables of the constraint new names based on the rename map *)
        val rename : t -> Atom_.Polynomial_.RenameMap_.t -> t

        (** Assigns each variable a value and returns if the constraint is satisfied for those values *)
        val eval_bool : t -> Atom_.Polynomial_.Valuation_.t -> bool

          
        (** The result of the following drop methods is not equivalent to the input constraint. 
            But each satisfying valuation of the input constraint is still a model of the new constraint. *)
          
        (** Drops all nonlinear atoms from the constraints. 
            Example: (a > 0 && b^2 < 2) gets transformed to (a > 0) *)
        val drop_nonlinear : t -> t

        (** Drops all atoms which use an unequal comparator.
            Example: (a > 0 && b <> 0) gets transformed to (a > 0) *)
        val drop_not_equal : t -> t

        (** Returns an constraint which only uses the LT comparator (<).
            This constraint is equivalent if it does not contain unequal comparators. 
            Otherwise it has the effect of a drop operator. *)
        val to_less_equal : Atom_.t -> t
    end
