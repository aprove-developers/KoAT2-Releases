open! OurBase

(** The state monad: a computation takes a state and returns its result together with the updated state. *)
module Make (State : sig
  type t
end) =
struct
  include Monad.Make (struct
    type 'a t = State.t -> 'a * State.t

    let map f m = Tuple2.map1 f % m
    let pure a state = (a, state)

    let bind m f state =
      let a, state = m state in
      f a state
  end)

  let get state = (state, state)
  let put state _ = ((), state)
  let modify f state = ((), f state)

  (** The result and the final state of the computation starting in [state] *)
  let run m state = m state

  (** The result of the computation starting in [state] *)
  let eval m = Tuple2.first % run m
end
