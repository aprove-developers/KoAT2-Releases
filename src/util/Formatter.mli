open Batteries
open FormatMonad

type format = Plain | Html | Markdown

val is_html: format -> bool
val is_plain: format -> bool
val is_markdown: format -> bool

val str: String.t -> unit Monad.t
val raw_str: String.t -> unit Monad.t
val str_line: String.t -> unit Monad.t

val str_header: FormattedString.header_size -> String.t -> unit Monad.t
val str_header_big: String.t -> unit Monad.t
val str_header_small: String.t -> unit Monad.t

val paragraph_formatted: FormattedString.t -> unit Monad.t

val paragraph: 'a Monad.t -> 'a Monad.t

val newline: unit Monad.t
val str_paragraph: String.t -> unit Monad.t
val title: String.t -> unit Monad.t

(** Renders the formatter Monad, with initially no metadata *)
val render_default: format:format -> unit Monad.t -> string
