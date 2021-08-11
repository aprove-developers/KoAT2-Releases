open Batteries

let compute = ref false
let format = ref Formatter.Plain
let proof = ref FormattedString.Empty

let compute_proof b = compute := b

let proof_format f = format := f

let add_to_proof str_computation =
  if !compute then
    proof := FormattedString.(!proof<>str_computation ())

let add_to_proof_with_format str_computation =
  if !compute then
    proof := FormattedString.(!proof<>str_computation !format)

let add_str_paragraph_to_proof f =
  add_to_proof @@ (FormattedString.mk_paragraph % FormattedString.mk_str_line % f )

let print_proof format =
  print_string @@ Formatter.render_default ~format (FormatMonad.write_format !proof)
