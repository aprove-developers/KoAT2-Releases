open! OurBase
open ProbabilisticProgramModules
open Bounds
open Approximation.Probabilistic

type elcb_map = (GRV.t, RationalBound.t, GRV.comparator_witness) Map.t

val trivial_sizebounds :
  Program.t ->
  grvs_in:GRV.t list ->
  elcb_map ->
  ClassicalApproximation.t ->
  ExpApproximation.t ->
  ExpApproximation.t

val nontrivial_sizebounds :
  Program.t ->
  program_vars:VarSet.t ->
  scc:GeneralTransitionSet.t ->
  rvts_scc:GRV.transition list ->
  elcb_map ->
  ClassicalApproximation.t ->
  ExpApproximation.t ->
  ExpApproximation.t

val propagate_sizes :
  Program.t ->
  program_vars:VarSet.t ->
  rvts_scc:GRV.transition list ->
  ClassicalApproximation.t ->
  ExpApproximation.t ->
  ExpApproximation.t
(** propagate sizes through nonprobabilistic updates *)
