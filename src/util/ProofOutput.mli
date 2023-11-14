val add_to_proof : (unit -> FormattedString.t) -> unit
(** Lazily adds a formatted string proof *)

val add_to_proof_with_format : (Formatter.format -> FormattedString.t) -> unit
(** like add_to_proof but may depend on selected output format. Can be used for embeddings graphs in HTML output. *)

val add_str_paragraph_to_proof : (unit -> string) -> unit
val print_proof : Formatter.format -> unit

(** Useful for Subproofs *)
module LocalProofOutput : sig
  type t
  type 'a with_proof = { result : 'a; proof : t }

  val result : 'a with_proof -> 'a
  val proof : 'a with_proof -> t
  val create : unit -> t
  val copy : t -> t
  val add_to_proof : t -> (unit -> FormattedString.t) -> unit
  val add_to_proof_with_format : t -> (Formatter.format -> FormattedString.t) -> unit
  val add_str_paragraph_to_proof : t -> (unit -> string) -> unit
  val get_proof : t -> Formatter.format -> FormattedString.t
end

val get_global_proof_as_local_proof : unit -> LocalProofOutput.t
(** Get a copy of the global proof as LocalProof *)

val set_global_proof_from_local_proof : LocalProofOutput.t -> unit
(** Set the proof generating function as copy of the local proof.
    Useful in conjunction with [get_as_local_proof_output] to switch between global proofs as used in CFR. *)
