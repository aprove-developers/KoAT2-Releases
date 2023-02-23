open Batteries

include Monad.Make(
  struct
    include Option.Monad

    type 'a t = 'a Option.t

    let map = Option.map
    let pure = Option.Monad.return

  end
)

