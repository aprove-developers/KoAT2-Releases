open Batteries

(** Provides all module types related to preprocessors *)

(** An atom is a comparison between two polynomials *)
module type Preprocessor =
  sig
    (** Transforms the transition graph in an equivalent form, which is more suitable for the upcoming computations. *)
    val transform : Program.t -> Program.t
  end
