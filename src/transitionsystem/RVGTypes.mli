(** Provides all modules related to result variable graphs. *)
open Batteries
open ProgramTypes

(** Module handling result variables. *)
module RV :
sig
  (** Module handling result variables. *)

  (** Type of a result variable is a transiton and a variable. *)
  type t = Transition.t * Var.t

  (** TODO doc *)
  val same : t -> t -> bool

  (** TODO doc *)
  val equivalent : t -> t -> bool

  (** TODO doc *)
  val compare_same : t -> t -> int

  (** TODO doc *)
  val compare_equivalent : t -> t -> int

  (** Returns a string representing a result variable consisting of the transition-id string and the variable string. *)
  val to_id_string : t -> string

  (** Generates a hash value for the result variable. *)
  val hash : t -> int

  (** Returns the transition of the result variable. *)
  val transition : t -> Transition.t

  (** Returns the variable of the result variable. *)
  val variable : t -> Var.t
end

(** Module handling result variable graphs. *)
module RVG :
sig
  include module type of Graph.Persistent.Digraph.ConcreteBidirectional(struct
                             include RV
                             let equal = same
                             let compare = compare_same
                           end)
  (** Module handling result variable graphs, i.e., a digraph where the nodes are result variables. *)

  (** Returns a string which is created by calling [to_id_string] on every result variable. *)
  val rvs_to_id_string : RV.t list -> string

  (** Returns the predecessors of a result variable in the result variable graph. *)
  val pre : t -> RV.t -> RV.t Enum.t

  type scc = RV.t list

  (** Compute the result variable graph *)
  val rvg : Program.t -> t

  (** Compute the result variable graph and lazily compute the list of all SCCs *)
  val rvg_with_sccs : Program.t -> t * scc list Lazy.t

end
