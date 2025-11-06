open Koat2
open! OurBase
open ProgramModules

let description = "Translate an input in the ari format to an equivalent input in the KoAT format "
let command = "ari-to-koat"

type params = {
  input_file : string; [@pos 0]  (** The input file which should be translated *)
  output_file : string option; [@aka [ "o" ]]
  parse_only : bool;  (** Do not print the KoAT version *)
}
[@@deriving cmdliner, show]

let run (params : params) =
  let program =
    match AriParser.from_file params.input_file with
    | Ok p -> p
    | Error s -> failwith ("ParseError: " ^ s)
  in
  if not params.parse_only then
    Program.to_file ~file:params.output_file program
