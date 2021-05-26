open Batteries

type header_size = Small | Big

type t = Empty
       | Str of String.t
       (* no escaping*)
       | RawStr of String.t
       | NewLine
       | Header of header_size * t
       | Paragraph of t
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

let mk_paragraph form = Paragraph form

type metadata = {
  title: String.t Option.t;
}

let set_title str = fun meta -> { title = Some str; }


let rec render_string f =
  match f with
  | Empty                   -> ""
  | Str s                   -> s
  | RawStr s                -> s
  | Paragraph f'            -> render_string f' ^ "\n\n"
  | NewLine                 -> "\n"
  | Header (_,f')           -> render_string f' ^ "\n"
  | SequentialComp (f1, f2) -> render_string f1 ^ render_string f2
