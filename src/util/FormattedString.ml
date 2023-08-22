open! OurBase

type header_size = Smallest | Smaller | Small | Big

type t =
  | Empty
  | Str of String.t
  (* no escaping*)
  | RawStr of String.t
  | NewLine
  | Header of header_size * t
  | Paragraph of t
  | Block of t
  | SequentialComp of t * t

let ( <> ) form1 form2 =
  match (form1, form2) with
  | Empty, f -> f
  | f, Empty -> f
  | f1, f2 -> SequentialComp (f1, f2)


let format_append = ( <> )
let mappend forms = List.fold_left ~f:(fun f f' -> SequentialComp (f, f')) ~init:Empty forms
let mk_raw_str str = RawStr str
let mk_str str = Str str
let mk_newline = NewLine
let mk_str_line str = mk_str str <> mk_newline
let mk_header size f = Header (size, f)
let mk_header_smallest f = Header (Smallest, f)
let mk_header_smaller f = Header (Smaller, f)
let mk_header_small f = Header (Small, f)
let mk_header_big f = Header (Big, f)
let mk_str_header_big = mk_header_big % mk_str
let mk_str_header_small = mk_header_small % mk_str
let mk_paragraph form = Paragraph form
let mk_block form = Block form

type metadata = { title : String.t Option.t }

let set_title str meta = { title = Some str }

let map_t_field f = function
  | Empty as e -> e
  | Str _ as s -> s
  | RawStr _ as s -> s
  | Paragraph t -> Paragraph (f t)
  | Block t -> Block (f t)
  | NewLine as n -> n
  | Header (s, t) -> Header (s, f t)
  | SequentialComp (t1, t2) -> SequentialComp (f t1, f t2)


(** Supply negative arguments to increase header sizes *)
let rec reduce_header_sizes ?(levels_to_reduce = 1) t =
  let size_to_int = function
    | Big -> 3
    | Small -> 2
    | Smaller -> 1
    | Smallest -> 0
  in
  let int_to_size = function
    | i when i >= 3 -> Big
    | 2 -> Small
    | 1 -> Smaller
    | _ -> Smallest
  in
  match t with
  | Header (size, t) -> Header (int_to_size (size_to_int size - levels_to_reduce), reduce_header_sizes t)
  | s -> map_t_field (reduce_header_sizes ~levels_to_reduce) s


let render_string =
  let rec helper ~indent f =
    match f with
    | Empty -> ""
    | Str s -> String.make indent ' ' ^ s
    | RawStr s -> s
    | Paragraph f' -> helper f' ~indent:(indent + 2) ^ "\n\n"
    | Block f' -> helper f' ~indent:(indent + 2) ^ "\n"
    | NewLine -> "\n"
    | Header (_, f') -> helper f' ~indent ^ "\n"
    | SequentialComp (f1, f2) -> helper f1 ~indent ^ helper ~indent f2
  in
  helper ~indent:0
