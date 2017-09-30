open Batteries

(** A classified bound is a bound of a certain form.
    The different classifications are not disjunctive.
    The upcoming classification set always includes the previous one. *)
type classification =
  (** Always smaller or equal to a constant or the value of a prevariable. Examples: x'=x , x'=y , x'=2
      max [c;x1;...;xn]) *)
  | Equality of int
  (** Always smaller or equal to the value of a prevariable plus a constant. Examples: x'=x+1 , x'=y+2 
      d + max [x1;...;xn] *)
  | AddsConstant of int
  (** Always smaller or equal to a scaling factor multiplied with the sum of all prevariables and a constant. Examples: x'=x+y , x'=2*(x+y+z) 
      s * (e + sum [x1;...;xn]) *)
  | ScaledSum of int * int
  (** Always smaller or equal to infinity *)
  | Unbound [@@deriving eq]
  
type t = classification * Var.t list
                
val as_bound : t -> Bound.t 
