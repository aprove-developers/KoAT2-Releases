open Batteries

include module type of Polynomials.Make(Polynomials.Make(PolyTypes.OurInt))
             
val flatten : Polynomials.Make(Polynomials.Make(PolyTypes.OurInt)).t -> Polynomials.Make(PolyTypes.OurInt).t
