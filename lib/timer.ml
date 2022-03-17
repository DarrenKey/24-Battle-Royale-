let noti_time time_left combos =
  Printf.printf "%N seconds left! Enter a solution for %s%!:" time_left
    combos

(* Helper function for timer to do action [noti_time] whenever [incr]
   seconds have passed.

   Requires: incr > 0, time_limit > 0*)
let rec incr_timer (incr : int) combos (time_left : int) =
  Lwt.bind
    (incr |> float_of_int |> Lwt_unix.sleep)
    (fun () ->
      noti_time time_left combos;
      incr_timer incr combos (time_left - incr))

let failed = print_endline "Wrong!"

(* Main timer function. Calls [incr_timer] to repeatedly do an action
   whenever a certain amount of time has passed.

   Requires: incr > 0, time_limit > 0*)
let timer ?(incr : int = 1) ?(time_limit : int = 20) (combos : string) =
  let repeated_timer = incr_timer incr combos time_limit in
  Lwt.bind
    (time_limit |> float_of_int |> Lwt_unix.sleep)
    (fun () ->
      Lwt.cancel repeated_timer;
      failed;
      Lwt.return ())
