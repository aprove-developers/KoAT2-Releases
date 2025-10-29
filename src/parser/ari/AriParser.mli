open! OurBase
open ProgramModules

exception AriParserExn of string

val from_file : String.t -> (Program.t, String.t) Result.t
val from_file_exn : String.t -> Program.t
