include Monad.Make(
  struct
    type 'a t = 'a Option.t

    let map = Option.map
    let pure = OurBase.Option.return
    let bind a f = OurBase.Option.bind a ~f
  end
)

