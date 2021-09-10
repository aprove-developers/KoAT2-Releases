open Batteries
open FormatMonad
open Formatter
open FormattedString

let compute = ref false
let format = ref Plain
let proof = ref Empty

let is_computing_proof = !compute

let compute_proof b = compute := b

let proof_format f = format := f

let add_to_proof str_computation =
  if !compute then
    proof := !proof<>str_computation ()

let add_to_proof_with_format str_computation =
  if !compute then
    proof := !proof<>str_computation !format

let add_str_paragraph_to_proof f =
  add_to_proof @@ (FormattedString.mk_paragraph % FormattedString.mk_str_line % f )

let print_proof format =
  print_string @@ Formatter.render_default ~format
    (write_format !proof >> title "KoAT2 Proof Output")
