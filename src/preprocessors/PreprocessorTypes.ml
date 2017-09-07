open Batteries

(** Provides all module types related to preprocessors *)

(** An atom is a comparison between two polynomials *)
module type Preprocessor =
  sig
    module TransitionGraph_ : TransitionGraphTypes.TransitionGraph

    (** Transforms the transition graph in an equivalent form, which is more suitable for the upcoming computations. *)
    val transform : TransitionGraph_.t -> TransitionGraph_.t
  end
