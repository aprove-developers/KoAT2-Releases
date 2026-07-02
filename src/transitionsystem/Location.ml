open! OurBase

module Inner = struct
  type t = string [@@deriving eq, ord, sexp]

  let to_string l = l
  let hash l = Hashtbl.hash l
  let of_string name = name

  let cmdliner_converter : t Cmdliner.Arg.conv =
    let parser = fun s -> Ok (of_string s) in
    let printer = fun fmt t -> Format.fprintf fmt "%s" (to_string t) in
    Cmdliner.Arg.conv (parser, printer)
end

include Inner
include Comparator.Make (Inner)
