open Batteries

type header_size = Small | Big

type t = Empty
       | Str of String.t
       (* no escaping*)
       | RawStr of String.t
       | NewLine
       | Header of header_size * t
       | Paragraph of t
       | Block of t
       | SequentialComp of t * t

let (<>) form1 form2 =
  match (form1, form2) with
  | (Empty, f) -> f
  | (f, Empty) -> f
  | (f1, f2) -> SequentialComp(f1, f2)

let format_append = (<>)
let mappend forms = List.fold_left (fun f f' -> SequentialComp (f,f')) Empty forms

let mk_raw_str str = RawStr str
let mk_str str = Str str

let mk_newline = NewLine

let mk_str_line str = mk_str str <> mk_newline

let mk_header size f = Header (size, f)

let mk_header_small f = Header (Small, f)
let mk_header_big f = Header (Big, f)

let mk_str_header_big = mk_header_big % mk_str
let mk_str_header_small = mk_header_small % mk_str

let mk_paragraph form = Paragraph form

let mk_block form = Block form

type metadata = {
  title: String.t Option.t;
}

let set_title str = fun meta -> { title = Some str; }


let render_string =
  let rec helper ~indent f =
    match f with
    | Empty                   -> ""
    | Str s                   -> String.repeat " " indent^s
    | RawStr s                -> s
    | Paragraph f'            -> helper f' ~indent:(indent + 2)^ "\n\n"
    | Block f'                -> helper f' ~indent:(indent + 2)^ "\n"
    | NewLine                 -> "\n"
    | Header (_,f')           -> helper f' ~indent ^ "\n"
    | SequentialComp (f1, f2) -> helper f1 ~indent ^ helper ~indent f2
  in
  helper ~indent:0
