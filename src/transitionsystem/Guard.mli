open Batteries

include module type of Constraints.Constraint

(** Simplifies the constraint by keeping a minimal equivalent set of atoms.
 * This method makes O(n^2) calls to the smt solver where n is the number of atoms in the guard.
 * Useful for debugging purposes.
 *)
val simplify_guard : t -> t

val to_string: ?pretty:bool -> t -> string
val to_file_string: t -> string
