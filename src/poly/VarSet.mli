open Batteries

include module type of Set.Make(Var)
   
val map_to_set : (elt -> 'b) -> t -> 'b Set.t

val map_to_list : (elt -> 'b) -> t -> 'b list

val map_to_array : (elt -> 'b) -> t -> 'b array

val to_string : t -> string

val of_string_list : string list -> t

val powerset : t -> t Enum.t

val combinations : int -> t -> t Enum.t

val sorted_combinations : int -> t -> t Enum.t

