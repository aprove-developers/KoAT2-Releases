open! OurBase

module Inner = struct
  type t = string [@@deriving eq, ord, sexp]

  let to_string l = l
  let hash l = Hashtbl.hash l
  let of_string name = name

  let cmdliner_converter : t Cmdliner.Arg.parser * t Cmdliner.Arg.printer =
    let of_string : t Cmdliner.Arg.parser = fun s -> `Ok (of_string s) in
    let printer : t Cmdliner.Arg.printer = fun fmt t -> Format.fprintf fmt "%s" (to_string t) in
    (of_string, printer)
end

include Inner
include Comparator.Make (Inner)
