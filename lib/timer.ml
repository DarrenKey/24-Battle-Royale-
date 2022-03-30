open Lwt

(* Helper function for timer to increment whenever [incr] seconds have
   passed.

   Requires: incr > 0, time_limit > 0*)
let rec incr_timer (incr : int) combos time_left notify =
  bind
    (incr |> float_of_int |> Lwt_unix.sleep)
    (fun () ->
      time_left := !time_left - incr;
      incr_timer incr combos time_left notify)

let time_limit = ref 40

(* Main timer function. Calls [incr_timer] to repeatedly do an action
   whenever a certain amount of time has passed.

   Requires: incr > 0, time_limit > 0*)
let timer
    ?(incr : int = 1)
    (notify : bool ref)
    (combos : string)
    (func_to_run : unit -> unit) =
  let repeated_timer = incr_timer incr combos time_limit notify in

  ( bind
      (!time_limit |> float_of_int |> Lwt_unix.sleep)
      (fun () ->
        cancel repeated_timer;
        time_limit := 40;
        func_to_run ();
        return ()),
    repeated_timer )
