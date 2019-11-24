open ProgramTypes

type metering_cache
val new_cache: unit -> metering_cache

val test : metering_cache -> Program.t -> unit
val find : metering_cache -> Program.t -> unit