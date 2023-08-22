open! OurBase

let start_time_of_koat2 = ref 0.

let set_timer tsecs =
  ignore (Unix.setitimer Unix.ITIMER_REAL { Unix.it_interval = 0.0; Unix.it_value = tsecs })


exception Timeout

let timed_run tsecs ?(action = const ()) command =
  let oldsig = Sys.signal Sys.sigalrm (Sys.Signal_handle (fun x -> raise Timeout)) in
  try
    let time = Unix.gettimeofday () in
    set_timer tsecs;
    let res = command () in
    set_timer 0.0;
    Sys.set_signal Sys.sigalrm oldsig;
    Some (res, Unix.gettimeofday () -. time)
  with
  | Timeout ->
      Sys.set_signal Sys.sigalrm oldsig;
      action ();
      None
