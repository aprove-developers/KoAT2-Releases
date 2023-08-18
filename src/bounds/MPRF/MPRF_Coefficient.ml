open Batteries
open Bounds

let logger = Logging.(get Time)

let rec factorial n =
  OurInt.(
    if is_zero n || equal one n then
      one
    else
      n * factorial (n - one))


let rec gamma n =
  if OurInt.equal OurInt.one n then
    OurRational.one
  else
    OurRational.(
      of_float 2.0
      + (gamma OurInt.(n - one) / of_ourint OurInt.(n - one))
      + (one / of_ourint (factorial OurInt.(n - one))))


let coefficient depth =
  OurRational.(gamma (OurInt.of_int depth) * of_ourint (factorial (OurInt.of_int depth)))
  |> OurRational.ceil
  |> tap (fun coeff ->
         Logger.log logger Logger.INFO (fun () -> ("coefficient ", [ ("beta", OurInt.to_string coeff) ])))
