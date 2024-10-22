open! OurBase

let direction_to_string_time = function
  | `ExpTime -> "expected time bound"
  | `ClassTime -> "classical time bound"


let time_direction = function
  | `ExpTimeClassSize -> `ExpTime
  | `ClassTimeExpSize -> `ClassTime
  | `ClassTimeClassSize -> `ClassTime


let size_direction = function
  | `ExpTimeClassSize -> `ClassSize
  | `ClassTimeExpSize -> `ExpSize
  | `ClassTimeClassSize -> `ClassSize


let direction_to_string_size = function
  | `ClassSize -> "classical size bounds"
  | `ExpSize -> "expected size bounds"


let direction_to_string dir =
  direction_to_string_time (time_direction dir) ^ " and " ^ direction_to_string_size (size_direction dir)
