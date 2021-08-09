open Batteries
open FormatMonad
open FormattedString

type format = Plain
            | Html
            | Markdown


let format_to_string = function
  | Plain -> "plain"
  | Html -> "html"
  | Markdown -> "markdown"

let is_html = function
  | Html -> true
  | _    -> false

let is_plain = function
  | Plain -> true
  | _    -> false

let is_markdown = function
  | Plain -> true
  | _    -> false

let str = write_format % mk_str
let raw_str = write_format % mk_raw_str
let str_line = write_format % mk_str_line
let str_header size str = write_format (mk_header size @@ mk_str str)
let str_header_big str = str_header Big str
let str_header_small str = str_header Small str

let paragraph_formatted = write_format % mk_paragraph

let paragraph (a: 'a FormatMonad.t): ('a FormatMonad.t) =
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

let render_plain _ f = render_string f

let rec render_markdown meta f =
  match f with
  | Empty          -> ""

  (* TODO: Is this a useful encoding for strings in markdown? *)
  | Str s                   -> Netencoding.Html.encode ~in_enc:`Enc_utf8 () @@
        String.replace_chars
          (fun c -> match c with
            | '*' -> "\\*"
            | c -> String.of_char c)
          s

  | RawStr s       -> s
  | Paragraph f'   -> render_markdown meta f' ^ "\n\n"
  | NewLine        -> "  \n"
  | Header (s,f')  ->
      (match s with
      | Big -> "\n# " ^ render_markdown meta f' ^ "\n"
      | Small -> "\n## " ^ render_markdown meta f' ^ "\n")
  | SequentialComp (f1, f2) -> render_markdown meta f1 ^ render_markdown meta f2

let render_monad ~(format: format) (expr: 'a FormatMonad.t) initial_meta =
  let (meta, form, _) = expr initial_meta in
  match format with
  | Plain -> render_plain meta form ^ "\n"
  | Html  -> render_html  meta form ^ "\n"
  | Markdown  -> render_markdown  meta form ^ "\n"

let render_default ~format expr = render_monad ~format:format expr initial_meta
