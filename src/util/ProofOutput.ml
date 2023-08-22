open! OurBase
open FormatMonad
open Formatter
open FormattedString

type proof_t = (Formatter.format -> FormattedString.t) ref
type t = proof_t

let create () = ref (fun format -> Empty)

let add_to_proof_with_format t str_computation =
  t :=
    let prev_proof = !t in
    fun format -> prev_proof format <> str_computation format


let add_to_proof t str_computation = add_to_proof_with_format t (fun format -> str_computation ())

let add_str_paragraph_to_proof t f =
  add_to_proof t @@ (FormattedString.mk_paragraph % FormattedString.mk_str_line % f)


module LocalProofOutput = struct
  type t = proof_t
  type 'a with_proof = { result : 'a; proof : t }

  let result with_proof = with_proof.result
  let proof with_proof = with_proof.proof
  let create : unit -> t = create
  let add_to_proof = add_to_proof
  let add_to_proof_with_format = add_to_proof_with_format
  let add_str_paragraph_to_proof = add_str_paragraph_to_proof
  let get_proof t format = !t format
  let copy t = ref !t
end

let proof : t = create ()
let add_to_proof = add_to_proof proof
let add_to_proof_with_format = add_to_proof_with_format proof
let add_str_paragraph_to_proof = add_str_paragraph_to_proof proof

let print_proof format =
  print_string @@ Formatter.render_default ~format (write_format (!proof format) >> title "KoAT2 Proof Output")
