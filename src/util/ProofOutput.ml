open Batteries

let compute = ref false
let proof = ref FormattedString.Empty

let compute_proof b = compute := b

let add_to_proof str_computation =
  if !compute then
    proof := FormattedString.(!proof<>str_computation ())

let add_str_paragraph_to_proof f =
  add_to_proof @@ (FormattedString.mk_paragraph % FormattedString.mk_str_line % f )

let print_proof () =
  print_string @@ FormattedString.render_string !proof
