module type Monad = sig
  type 'a t
  val pure: 'a -> 'a t
  val return: 'a -> 'a t

  val bind: 'a t -> ('a -> 'b t) -> 'b t
  val (>>=): 'a t -> ('a -> 'b t) -> 'b t
  val (>>): 'a t -> 'b t -> 'b t

  (** Functorial let binding ([ map ]) ) *)
  val (let+): 'a t -> ('a -> 'b) -> 'b t

  (** Applicative and binding *)
  val (and+): 'a t -> 'b t -> ('a * 'b) t

  (** Monadic let binding ([ bind ]) *)
  val (let*): 'a t -> ('a -> 'b t) -> 'b t
  val (and*): 'a t -> 'b t -> ('a * 'b) t

  val when_m: bool -> unit t -> unit t

  val sequence: 'a t list -> 'a list t

  val mapM: ('a -> 'b t) -> 'a list -> 'b list t

  val liftM2: ('a -> 'b -> 'c) -> 'a t -> 'b t -> 'c t
end
