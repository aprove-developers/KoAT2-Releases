open Batteries
open Big_int.Infix

module MakeOverIndeterminate(I: PolyTypes.Indeterminate)(Value : PolyTypes.Ring) =
  struct
    type valuation = Valuation.MakeOverIndeterminate(I)(Value).t
    module Monomial = Monomials.MakeOverIndeterminate(I)(Value)

    type indeterminate = I.t

    type t =
      {
        coeff : Value.t;
        mon :   Monomial.t;
      } [@@deriving eq, ord]

    let is_integral t =
      Value.is_integral t.coeff && Monomial.is_integral t.mon

    type monomial = Monomial.t

    type value = Value.t

    let make coefficient monomial = { coeff = coefficient; mon = monomial }

    let fold ~const ~indeterminate ~times ~pow scaled =
      times (const scaled.coeff) (Monomial.fold ~const ~indeterminate ~times ~pow scaled.mon)

    let coeff scaled = scaled.coeff

    let monomial scaled = scaled.mon

    let degree scaled = Monomial.degree scaled.mon

    let to_string ?(to_file = false) ?(pretty = false) scaled =
      if Value.(scaled.coeff =~= one) then Monomial.to_string ~to_file ~pretty scaled.mon
      else if Value.(scaled.coeff =~= zero) then ""
      else Value.to_string scaled.coeff ^ if Monomial.(scaled.mon =~= Monomial.one) then "" else (if pretty then "⋅" else "*") ^ Monomial.to_string ~to_file ~pretty scaled.mon

    type outer_t = t
    module BasePartialOrderImpl : (PolyTypes.BasePartialOrder with type t = outer_t) =
      struct
        type t = outer_t

        let (=~=) scaled1 scaled2 =
          Value.(scaled1.coeff =~= scaled2.coeff) && Monomial.(scaled1.mon =~= scaled2.mon)

        let (>) s1 s2 = match (s1, s2) with
          (* TODO Find some rules to compare *)
          | (s1, s2) -> if Monomial.(monomial s1 =~= monomial s2) && (coeff s1 > coeff s2) then
                          Some true
                        else None

      end
    include PolyTypes.MakePartialOrder(BasePartialOrderImpl)

    let rename varmapping scaled = { scaled with mon = Monomial.rename varmapping scaled.mon }

    let eval_f scaled f =
      Value.mul scaled.coeff (Monomial.eval_f scaled.mon f)

    let eval scaled valuation =
      Value.mul scaled.coeff (Monomial.eval scaled.mon valuation)

    let mult_with_const const scaled = { scaled with coeff = Value.mul scaled.coeff const }

    let mul scaled1 scaled2 =
      {
        coeff = Value.mul scaled1.coeff scaled2.coeff;
        mon = Monomial.mul scaled1.mon scaled2.mon
      }

    let one =
      {
        coeff = Value.one;
        mon = Monomial.one
      }

    let lift mon =
      {
        coeff = Value.one;
        mon = mon
      }

    let indeterminates scaled = Monomial.indeterminates scaled.mon

    let vars scaled = Monomial.vars scaled.mon

  end

module Make = MakeOverIndeterminate(VarIndeterminate)
