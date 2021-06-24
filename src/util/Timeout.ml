open Batteries

let start_time_of_koat2 = ref 0.

let set_timer tsecs =
  ignore (Unix.setitimer Unix.ITIMER_REAL { Unix.it_interval = 0.0; Unix.it_value = tsecs })

exception Timeout

let timed_run tsecs ?(action=lazy ()) command =
    match command with
    | None -> None
    | Some call -> 
        let oldsig = Sys.signal Sys.sigalrm (Sys.Signal_handle (fun x -> raise Timeout)) in
            try
            set_timer tsecs;
            let res = (Lazy.force call) in
                set_timer 0.0;
                Sys.set_signal Sys.sigalrm oldsig;
                (Some res)
            with
            | Timeout ->
                (
                Sys.set_signal Sys.sigalrm oldsig;
                (Lazy.force action);
                None
                ) 