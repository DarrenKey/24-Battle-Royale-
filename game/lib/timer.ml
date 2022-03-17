open Lwt

let noti_time_list = [ 40; 20; 5 ]

let noti_time time_left combos =
  if List.mem time_left noti_time_list then
    Lwt_io.printf "\n%N seconds left!\n>%!" time_left |> ignore
  else ()

(* Helper function for timer to do action [noti_time] whenever [incr]
   seconds have passed.

   Requires: incr > 0, time_limit > 0*)
let rec incr_timer (incr : int) combos (time_left : int) =
  bind
    (incr |> float_of_int |> Lwt_unix.sleep)
    (fun () ->
      noti_time time_left combos;
      incr_timer incr combos (time_left - incr))

(* Main timer function. Calls [incr_timer] to repeatedly do an action
   whenever a certain amount of time has passed.

   Requires: incr > 0, time_limit > 0*)
let timer
    ?(incr : int = 1)
    ?(time_limit : int = 40)
    (combos : string)
    (func_to_run : unit -> unit) =
  let repeated_timer = incr_timer incr combos (time_limit - 1) in

  noti_time time_limit combos;
  ( bind
      (time_limit |> float_of_int |> Lwt_unix.sleep)
      (fun () ->
        cancel repeated_timer;
        func_to_run ();
        return ()),
    repeated_timer )
