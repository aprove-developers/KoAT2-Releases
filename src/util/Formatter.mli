open Batteries
open FormatMonad

type format = Plain | Html | Markdown

val is_html: format -> bool
val is_plain: format -> bool
val is_markdown: format -> bool

val str: String.t -> unit FormatMonad.t
val raw_str: String.t -> unit FormatMonad.t
val str_line: String.t -> unit FormatMonad.t

val str_header: FormattedString.header_size -> String.t -> unit FormatMonad.t
val str_header_big: String.t -> unit FormatMonad.t
val str_header_small: String.t -> unit FormatMonad.t

val paragraph_formatted: FormattedString.t -> unit FormatMonad.t

val paragraph: 'a FormatMonad.t -> 'a FormatMonad.t

val newline: unit FormatMonad.t
val str_paragraph: String.t -> unit FormatMonad.t
val title: String.t -> unit FormatMonad.t

(** Renders the formatter Monad, with initially no metadata *)
val render_default: format:format -> unit FormatMonad.t -> string
