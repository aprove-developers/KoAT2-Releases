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

        module Infix : sig
          val (>) : Polynomial_.t -> Polynomial_.t -> t
          val (>=) : Polynomial_.t -> Polynomial_.t -> t
          val (<) : Polynomial_.t -> Polynomial_.t -> t
          val (<=) : Polynomial_.t -> Polynomial_.t -> t
        end

        (** Following methods return certain properties of the atom. *)
          
        val (=~=) : t -> t -> bool

        val neg : t -> t
          
        val to_string : t -> string

        (** Returns if both polynomials are linear. *)
        val is_linear : t -> bool

        (** Returns the set of variables which are active in the atom.
            A variable is active, if it's value has an effect on the evaluation of the atom. *)
        val vars : t -> Var.t Set.t

        (** Returns a normalised form of the atom, where the returned polynomial represents the atom in the form p <= 0. *)
        val normalised_lhs : t -> Polynomial_.t
          

        (** Following methods manipulate atoms and return the manipulated versions. *)

        (** Assigns the variables of the atom new names based on the rename map. *)
        val rename : t -> Polynomial_.RenameMap_.t -> t

          (*
        (** Assigns each variable a value and returns if the atom is satisfied for those values. *)
        val models : t -> Polynomial_.Valuation_.t -> bool
           *)

        (** Replaces all operations by new constructors. *)
        val fold : const:(Polynomial_.Value.t -> 'b) ->
                   var:(Var.t -> 'b) ->
                   neg:('b -> 'b) ->               
                   plus:('b -> 'b -> 'b) ->
                   times:('b -> 'b -> 'b) ->
                   pow:('b -> int -> 'b) ->
                   le:('b -> 'b -> 'c) ->
                   t -> 'c 
          
    end

(** A constraint is a conjunction of atoms *)
module type Constraint =
  sig
        module Polynomial_ : Polynomial

        module Atom_ : Atom with module Polynomial_ = Polynomial_

        type t
        

        (** Following methods are convenience methods for the creation of atoms. *)
          
        val lift : Atom_.t -> t      
        val mk : Atom_.t list -> t
        val mk_true : t
        val mk_and : t -> t -> t

        (** Creates a constraint that expresses the equality of the two polynomials. *)
        val mk_eq : Polynomial_.t -> Polynomial_.t -> t

        val mk_gt : Polynomial_.t -> Polynomial_.t -> t
        val mk_ge : Polynomial_.t -> Polynomial_.t -> t
        val mk_lt : Polynomial_.t -> Polynomial_.t -> t
        val mk_le : Polynomial_.t -> Polynomial_.t -> t

        module Infix : sig
          val (=) : Polynomial_.t -> Polynomial_.t -> t
          val (>) : Polynomial_.t -> Polynomial_.t -> t
          val (>=) : Polynomial_.t -> Polynomial_.t -> t
          val (<) : Polynomial_.t -> Polynomial_.t -> t
          val (<=) : Polynomial_.t -> Polynomial_.t -> t
          val (&&) : t -> t -> t
        end

        val all : t list -> t
          
        (** Following methods return certain properties of the constraint. *)
          
        (** Returns if the constraint is a tautology *)
        val is_true : t -> bool
          
        val (=~=) : t -> t -> bool
        
        val num_of_atoms : t -> int
          
        (** Returns the set of variables which are active in the constraint.
            A variable is active, if it's value has an effect on the evaluation of the constraint. *)
        val vars : t -> Var.t Set.t

        val to_string : t -> string


        (** Following methods manipulate atoms and return the manipulated versions. *)

        (** Assigns the variables of the constraint new names based on the rename map *)
        val rename : t -> Polynomial_.RenameMap_.t -> t

          (*
        (** Assigns each variable a value and returns if the constraint is satisfied for those values *)
        val models : t -> Polynomial_.Valuation_.t -> bool
           *)

        (** Replaces all operations by new constructors. *)
        val fold : const:(Polynomial_.Value.t -> 'b) ->
                   var:(Var.t -> 'b) ->
                   neg:('b -> 'b) ->               
                   plus:('b -> 'b -> 'b) ->
                   times:('b -> 'b -> 'b) ->
                   pow:('b -> int -> 'b) ->
                   le:('b -> 'b -> 'c) ->
                   correct:('d) ->
                   conj:('d -> 'c -> 'd) ->
                   t -> 'd

          
        (** The result of the following drop methods is not equivalent to the input constraint. 
            But each satisfying valuation of the input constraint is still a model of the new constraint. *)
          
        (** Drops all nonlinear atoms from the constraints. 
            Example: (a > 0 && b^2 < 2) gets transformed to (a > 0) *)
        val drop_nonlinear : t -> t
        
        (** Returns the row of all coefficients of a variable in a constraint...used for farkas quantor elimination*)
        val get_coefficient_vector : Var.t -> t -> Polynomial_.Value.t list
        
        val get_matrix : Var.t Set.t -> t -> Polynomial_.Value.t list list
        
        (** Returns the row of all coefficients of a variable in a constraint...used for farkas quantor elimination*)
        val get_constant_vector : t -> Polynomial_.Value.t list
        
        val dualise : Var.t list -> Polynomial_.Value.t list list -> Polynomial_.t list -> t
        
        val farkas_transform : t -> Atom_.t -> t
  end

(** A formula is a propositional formula *)
module type Formula =
  sig
        module Polynomial_ : Polynomial
        module Atom_ : Atom with module Polynomial_ = Polynomial_
        module Constraint_ : Constraint with module Polynomial_ = Polynomial_

        type t
        

        (** Following methods are convenience methods for the creation of atoms. *)
          
        val lift : Atom_.t -> t      
        val mk : Constraint_.t -> t      
        val disj : Constraint_.t list -> t      
        val mk_true : t
        val mk_and : t -> t -> t
        val mk_or : t -> t -> t

        (** Creates a constraint that expresses the equality of the two polynomials. *)
        val mk_eq : Polynomial_.t -> Polynomial_.t -> t
        val mk_gt : Polynomial_.t -> Polynomial_.t -> t
        val mk_ge : Polynomial_.t -> Polynomial_.t -> t
        val mk_lt : Polynomial_.t -> Polynomial_.t -> t
        val mk_le : Polynomial_.t -> Polynomial_.t -> t
        val mk_le_than_max : Polynomial_.t -> Polynomial_.t list -> t
          
        val all : t list -> t
        val any : t list -> t
          
        module Infix : sig
          val (=) : Polynomial_.t -> Polynomial_.t -> t
          val (>) : Polynomial_.t -> Polynomial_.t -> t
          val (>=) : Polynomial_.t -> Polynomial_.t -> t
          val (<) : Polynomial_.t -> Polynomial_.t -> t
          val (<=) : Polynomial_.t -> Polynomial_.t -> t
          val (&&) : t -> t -> t
          val (||) : t -> t -> t
        end

        (** Following methods return certain properties of the formula. *)
          
        (** Returns the set of variables which are active in the formula.
            A variable is active, if it's value has an effect on the evaluation of the constraint. *)
        val vars : t -> Var.t Set.t

        val to_string : t -> string


        (** Following methods manipulate atoms and return the manipulated versions. *)

        (** Assigns the variables of the constraint new names based on the rename map *)
        val rename : t -> Polynomial_.RenameMap_.t -> t

        (** Replaces all operations by new constructors. *)
        val fold : const:(Polynomial_.Value.t -> 'b) ->
                   var:(Var.t -> 'b) ->
                   neg:('b -> 'b) ->               
                   plus:('b -> 'b -> 'b) ->
                   times:('b -> 'b -> 'b) ->
                   pow:('b -> int -> 'b) ->
                   le:('b -> 'b -> 'c) ->
                   correct:('d) ->
                   conj:('d -> 'c -> 'd) ->
                   wrong:('e) ->
                   disj:('e -> 'd -> 'e) ->
                   t -> 'e
  end
