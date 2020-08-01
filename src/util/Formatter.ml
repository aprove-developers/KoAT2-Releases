open Batteries
open FormatMonad
open FormattedString

type format = Plain
            | Html

let str = write_format % mk_str
let raw_str = write_format % mk_raw_str
let str_line = write_format % mk_str_line
let str_header size str = write_format (mk_header size @@ mk_str str)
let str_header_big str = str_header Big str
let str_header_small str = str_header Small str

let paragraph_formatted = write_format % mk_paragraph

let paragraph (a: 'a Monad.t): ('a Monad.t) =
  fun meta ->
    let (meta', format', a') = a meta in
    (meta', mk_paragraph format', a')

let str_paragraph = write_format % mk_paragraph % mk_str

let newline = write_format mk_newline

let title = write_meta % set_title

let initial_meta = {title = None; }

let render_html meta f =
  let preamble = "<!DOCTYPE html>\n" in
  let header =
    "<html>\n<head>\n<title>" ^ Option.default "" meta.title ^ "</title>\n</head>\n"
  in
  let start_body = "<body>\n" in
  let end_body = "</body>\n" in
  let end_document = "</html>" in
  let rec render_f_only f' =
    match f' with
    | Empty                   -> ""
    | Str s                   -> Netencoding.Html.encode ~in_enc:`Enc_utf8 () s ^ "\n"
    | RawStr s                -> s ^ "\n"
    | Paragraph f''           -> "<p>\n" ^ render_f_only f'' ^ "</p>\n"
    | NewLine                 -> "<br/>\n"
    | Header (s,f')           ->
        (match s with
        | Big -> "<h3>" ^ render_f_only f' ^ "</h3>\n"
        | Small -> "<h4>" ^ render_f_only f' ^ "</h4>\n")
    | SequentialComp (f1, f2) -> render_f_only f1 ^ render_f_only f2
  in
  String.concat ""
    [
      preamble
    ; header
    ; start_body
    ; render_f_only f
    ; end_body
    ; end_document
    ]

let rec render_plain _ f = render_string f

let render_monad ~format (expr: 'a Monad.t) initial_meta =
  let (meta, form, _) = expr initial_meta in
  match format with
  | Plain -> render_plain meta form ^ "\n"
  | Html  -> render_html  meta form ^ "\n"

let render_default ~format expr = render_monad ~format:format expr initial_meta
