open Batteries
open ProgramTypes

module TransitionForExpectedSize =
  struct
    type t = GeneralTransition.t * Location.t

    let gt (gt, _) = gt

    let loc (_,l) = l

    let compare (gt1,l1) (gt2,l2) =
      if GeneralTransition.compare gt1 gt2 = 0 then
        Location.compare l1 l2
      else
        GeneralTransition.compare gt1 gt2

    let src: (t -> Location.t) = GeneralTransition.start % Tuple2.first

    let same (gt1,l1) (gt2,l2) =
      GeneralTransition.same gt1 gt2 &&
      Location.equal l1 l2

    let target_string: t -> string = Location.to_string % Tuple2.second

    let to_id_string (gt,l)=
      "(" ^ GeneralTransition.to_id_string gt ^ ", " ^
      Location.to_string l ^ ")"

    let to_string (gt,l)=
      "(" ^ GeneralTransition.to_string gt ^ ", " ^
      Location.to_string l ^ ")"

    let compare_same: t -> t -> int = compare
    let compare_equivalent: t -> t -> int = compare
    let equivalent: t -> t -> bool = same

  end


