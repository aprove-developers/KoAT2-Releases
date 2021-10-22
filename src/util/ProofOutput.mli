

(** Settings *)
val compute_proof: bool -> unit
val is_computing_proof: unit -> bool
val proof_format: Formatter.format -> unit
val get_format: unit -> Formatter.format

val add_to_proof: (unit -> FormattedString.t) -> unit
(** like add_to_proof but may depend on selected output format. Can be used for embeddings graphs in HTML output. *)
val add_to_proof_with_format: (Formatter.format -> FormattedString.t) -> unit
val add_str_paragraph_to_proof: (unit -> string) -> unit
val print_proof : Formatter.format -> unit
