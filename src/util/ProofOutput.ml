open Batteries
open FormatMonad
open Formatter
open FormattedString

let proof_enabled = ref false
let format = ref Plain
let proof: (FormattedString.t Lazy.t) ref = ref (Lazy.from_val Empty)

let proof_is_enabled () = !proof_enabled

let enable_proof b = proof_enabled := b

let proof_format f = format := f

let get_format () = !format

let add_to_proof str_computation =
  if !proof_enabled then
    proof :=  let p = !proof in lazy(Lazy.force p<> Lazy.force (Lazy.from_fun str_computation))

let add_to_proof_with_format str_computation =
  if !proof_enabled then
    proof := let p = !proof in lazy(Lazy.force p<>str_computation !format)

let add_str_paragraph_to_proof f =
  add_to_proof @@ (FormattedString.mk_paragraph % FormattedString.mk_str_line % f )

let print_proof format =
  print_string @@ Formatter.render_default ~format
    (write_format (Lazy.force !proof) >> title "KoAT2 Proof Output")
