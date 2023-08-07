open Bounds

(** Computes coefficients for Multiphase Ranking Functions necessary for upper time-bounds. *)
(** Handles multiphase ranking functions (see [Ben-Amram and Genaim 2017]) and computes coefficients for upper time-bounds. *)

val coefficient : int -> OurInt.t
(** Returns maximal coefficient for a Multiphase Ranking Function as in [Ben-Amram and Genaim 2017]. *)
(** The argument corresponds to the depth of the MPRF. *)
