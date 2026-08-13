open Koat2
open! OurBase

let mk_enum choices =
  let docv = String.concat ~sep:"|" (List.map ~f:fst choices) in
  Cmdliner.Arg.enum ~docv choices
