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
