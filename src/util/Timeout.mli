(**reference for storing the starttime of current analysis *)
val start_time_of_koat2: float ref

(**forces execution of the third argument by using timeout (in s) in first argument. The second argument is forced in case of reaching the timeout *)
val timed_run : float -> ?action:(unit -> unit) -> (unit -> 'a) -> ('a * float) option
