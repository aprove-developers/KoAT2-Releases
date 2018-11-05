open Polynomials
open Batteries
open Formulas

module Make_LocalSizeBoundHelperFunctions (Num : PolyTypes.OurNumber) 
                                          (Poly : 
                                            sig
                                              include PolyTypes.Polynomial with type value = Num.t 
                                                                            and type valuation = Valuation.Make(Num).t
                                                                            and type monomial = Monomials.Make(Num).t
                                              val max_of_occurring_constants : t -> Num.t
                                            end )
                                          (Form : 
                                            sig 
                                              include ConstraintTypes.Formula
                                              val max_of_occurring_constants : t -> Num.t
                                            end) = 
  struct
    (* For 's' it is sufficient to only view the max occurring constants of the update polynomial. *)
    let s_range update =
      update
      |> Poly.max_of_occurring_constants
      |> Num.max (Num.of_int 1) (* 0 or lower is not allowed *)
      |> Num.min (Num.of_int 1024) (* TODO We cut it at the moment at 1024, because sometimes the approximation is worse than an integer value. *)
      |> Num.to_int
  
    (* For 'c' we want to view the max occurring constants of the complete formula *)
    let c_range formula =
      formula
      |> Form.max_of_occurring_constants
      |> Num.min (Num.of_int 1024) (* TODO We cut it at the moment at 1024, because sometimes the approximation is worse than an integer value. *)
      |> Num.to_int

    (** Performs a binary search between the lowest and highest value to find the optimal value which satisfies the predicate.
        We assume that the highest value already satisfies the predicate.
        Therefore this method always finds a solution. *)
    let rec binary_search ?(divisor=2.) (lowest: int) (highest: int) (p: int -> bool) =
      if lowest >= highest then
        lowest
      else
        (* We need to ensure that the result is always round down to prevent endless loops.
           Normal integer division rounds towards zero. *)
        let newBound = Float.to_int (Float.floor (Float.div (Float.of_int (lowest + highest)) divisor)) in
        if p newBound then
          binary_search ~divisor:(if newBound < 0 then 2. else divisor) lowest newBound p
        else
          binary_search ~divisor:(if newBound < 0 then divisor else 2.) (newBound + 1) highest p        
  end
