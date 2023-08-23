open! OurBase
open ProbabilisticProgramModules

(** This module contains useful functions for the computation of probabilistic bounds. *)

val entry_gts_with_locs : Program.t -> GeneralTransitionSet.t -> (GeneralTransition.t * Location.t) Sequence.t
(** Obtain all entry general transition to the general transition set with corresponding entry locations *)

val entry_gts : Program.t -> GeneralTransitionSet.t -> GeneralTransitionSet.t
(** Obtain all entry general transition to the general transition set *)

val entry_locations : Program.t -> GeneralTransitionSet.t -> LocationSet.t
(** Obtain all entry locations of the general transition set *)
