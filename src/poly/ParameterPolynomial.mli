open Batteries

include module type of Polynomials.Make(Polynomials.Make(OurInt))
             
val flatten : Polynomials.Make(Polynomials.Make(OurInt)).t -> Polynomials.Make(OurInt).t
