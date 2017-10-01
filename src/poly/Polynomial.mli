open Batteries

include module type of Polynomials.Make(PolyTypes.OurInt)
             
(** Separates the polynomial in two polynomials 
    where the first one contains all the monomials with positive sign 
    and the second one contains all the monomials with negative sign. *)
val separate_by_sign : t -> (t * t)
      
