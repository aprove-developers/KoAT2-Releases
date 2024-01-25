open! OurBase

(** Generic type for abstractions
    A version is a location with an (possibly abstracted) constraint used in the partial evaluation graph *)
module Version (A : Abstraction.Abstraction) = struct
  module Inner = struct
    open Constraints

    type t = Location.t * A.t [@@deriving ord]

    let sexp_of_t = Sexplib0.Sexp_conv.sexp_of_opaque
    let _to_string (l, a) = Printf.sprintf "⟨%s, %s⟩" (Location.to_string l) (A.to_string ~pretty:false a)

    let to_string_pretty (l, a) =
      Printf.sprintf "⟨%s, %s⟩" (Location.to_string l) (A.to_string ~pretty:true a)


    let hash l = Hashtbl.hash l
    let mk location abstracted = (location, abstracted)
    let mk_true location = (location, A.Constr Constraint.mk_true)
    let location (l, _) = l
    let abstracted (_, a) = a
    let is_true (_, a) = A.is_true a
  end

  include Inner
  include Comparator.Make (Inner)
end
