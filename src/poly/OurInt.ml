(** Implementation of an integer-ring using Big_int. *)
open Batteries

include Number.MakeNumeric(Big_int)
(** Comparator for two integers. *)
let (=~=) = equal

(** Calculates i^n. *)
(* TODO Make it possible to pow with Big_int *)
let pow i (n: int) = pow i (of_int n)  

(** Returns maximum of first and second integer. *)
let max a b =
  if Compare.(a >= b) then
    a
  else
    b

(** Returns minimum of first and second integer. *)
let min a b =
  if Compare.(a <= b) then
    a
  else
    b
