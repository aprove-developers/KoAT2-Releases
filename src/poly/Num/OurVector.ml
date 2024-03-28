open OurBase
open Koat2_external.Algebraic.Algebraic

module VectorOver (Value : PolyTypes.Ring) = struct
  type 'a t = 'a vec

  let of_list = vec_of_list
  let to_list = list_of_vec
  let of_fun n f = vec (nat_of_integer @@ OurInt.of_int n) (f % OurInt.to_int % integer_of_nat)
  let map = map_vec
  let dim_vec vec = (OurInt.to_int % integer_of_nat % dim_vec) vec

  exception VectorIndexOutOfRange

  let index vec n =
    if n < 0 || n >= dim_vec vec then
      raise VectorIndexOutOfRange
    else
      vec_index vec (nat_of_integer @@ OurInt.of_int n)


  let to_string vec = Util.sequence_to_string ~f:Value.to_string (Sequence.of_list @@ to_list vec)

  let scalar_prod v w =
    let sum = Sequence.fold ~f:Value.add ~init:Value.zero in
    sum @@ Sequence.of_list @@ List.map (List.zip_truncate (to_list v) (to_list w)) ~f:(uncurry Value.mul)


  let equal v w = List.equal Value.equal (to_list v) (to_list w)
  let is_zero = List.for_all ~f:(fun x -> Value.(equal zero x)) % to_list
end

module CAVector = VectorOver (OurAlgebraicComplex)
