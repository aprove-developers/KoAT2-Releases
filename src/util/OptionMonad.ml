open Batteries

include Monad.Make(
  struct
    include Option.Monad

    type 'a t = 'a Option.t

    let pure = Option.Monad.return

  end
)

