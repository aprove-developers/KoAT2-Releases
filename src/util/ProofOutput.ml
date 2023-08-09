open OurBase
open FormatMonad
open Formatter
open FormattedString

type proof_t = FormattedString.t Lazy.t ref

let create () = ref (Lazy.from_val Empty)
let proof_enabled = ref false
let format = ref Plain
let proof : proof_t = create ()

let add_to_proof t str_computation =
  if !proof_enabled then
    t :=
      let p = !t in
      lazy (Lazy.force p <> str_computation ())


let add_to_proof_with_format t str_computation =
  if !proof_enabled then
    let format = !format in
    t :=
      let p = !t in
      lazy (Lazy.force p <> str_computation format)


let add_str_paragraph_to_proof t f =
  add_to_proof t @@ (FormattedString.mk_paragraph % FormattedString.mk_str_line % f)


module LocalProofOutput = struct
  type t = FormattedString.t Lazy.t ref

  let create = create
  let add_to_proof = add_to_proof
  let add_to_proof_with_format = add_to_proof_with_format
  let add_str_paragraph_to_proof = add_str_paragraph_to_proof
  let get_proof t : FormattedString.t = Lazy.force !t
end

let proof_is_enabled () = !proof_enabled
let enable_proof b = proof_enabled := b
let proof_format f = format := f
let get_format () = !format
let add_to_proof = add_to_proof proof
let add_to_proof_with_format = add_to_proof_with_format proof
let add_str_paragraph_to_proof = add_str_paragraph_to_proof proof

let print_proof format =
  print_string
  @@ Formatter.render_default ~format (write_format (Lazy.force !proof) >> title "KoAT2 Proof Output")
