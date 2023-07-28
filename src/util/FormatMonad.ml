open FormattedString

(* Monad instance *)
include Monad.Make(
  struct
(*     type 'a t = M of (metadata -> metadata * formatted * 'a) *)
    type 'a t = (metadata -> metadata * FormattedString.t * 'a)

    let map (f: 'a -> 'b) (a: 'a t): 'b t = OurBase.(Tuple3.map3 f % a)

    let pure a = fun metadata -> (metadata, Empty, a)

    let bind (a : 'a t) (g: 'a -> 'b t) = fun meta ->
      let (meta', form, a') = a meta in
      let (meta'', form', b) = g a' meta' in
      (meta'', form <> form', b)
    end
)

let write_format (f: FormattedString.t): unit t = fun meta -> (meta, f, ())

let write_meta (f: metadata -> metadata): unit t = fun meta -> let meta' = f meta in (meta', Empty, ())
