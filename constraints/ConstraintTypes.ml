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
              LE -> a <= b. *)
          type t = GT | GE | LT | LE

          (** Returns a list of all possible comparators *)
          val values : t list

          (** Returns a list of all possible comparators with their string representation *)
          val str_values : string list

          val to_string : t -> string

        end
                                                  
        type t
             

        (** Following methods are convenience methods for the creation of atoms. *)
          
        val mk : Comparator.t -> Polynomial_.t -> Polynomial_.t -> t
        val mk_gt : Polynomial_.t -> Polynomial_.t -> t
        val mk_ge : Polynomial_.t -> Polynomial_.t -> t
        val mk_lt : Polynomial_.t -> Polynomial_.t -> t
        val mk_le : Polynomial_.t -> Polynomial_.t -> t
        

        (** Following methods return certain properties of the atom. *)
          
        val (=~=) : t -> t -> bool
        
        val to_string : t -> string

        (** Returns if both polynomials are linear. *)
        val is_linear : t -> bool

        (** Returns the set of variables which are active in the atom.
            A variable is active, if it's value has an effect on the evaluation of the atom. *)
        val vars : t -> Polynomial_.Var.t Set.t

        (** Returns a normalised form of the atom, where the returned polynomial represents the atom in the form p <= 0. *)
        val normalised_lhs : t -> Polynomial_.t
          

        (** Following methods manipulate atoms and return the manipulated versions. *)

        (** Assigns the variables of the atom new names based on the rename map. *)
        val rename : t -> Polynomial_.RenameMap_.t -> t

        (** Assigns each variable a value and returns if the atom is satisfied for those values. *)
        val models : t -> Polynomial_.Valuation_.t -> bool
                            
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

        (** Creates a constraint that expresses the equality of the two polynomials. *)
        val mk_eq : Atom_.Polynomial_.t -> Atom_.Polynomial_.t -> t

        val mk_gt : Atom_.Polynomial_.t -> Atom_.Polynomial_.t -> t
        val mk_ge : Atom_.Polynomial_.t -> Atom_.Polynomial_.t -> t
        val mk_lt : Atom_.Polynomial_.t -> Atom_.Polynomial_.t -> t
        val mk_le : Atom_.Polynomial_.t -> Atom_.Polynomial_.t -> t


        val all : t list -> t
          
        (** Following methods return certain properties of the constraint. *)
          
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
        val models : t -> Atom_.Polynomial_.Valuation_.t -> bool

          
        (** The result of the following drop methods is not equivalent to the input constraint. 
            But each satisfying valuation of the input constraint is still a model of the new constraint. *)
          
        (** Drops all nonlinear atoms from the constraints. 
            Example: (a > 0 && b^2 < 2) gets transformed to (a > 0) *)
        val drop_nonlinear : t -> t

  end
