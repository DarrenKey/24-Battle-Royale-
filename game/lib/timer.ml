let noti_time_list = [ 20; 5 ]

let noti_time time_left combos =
  if List.mem time_left noti_time_list then
    Printf.printf "%N seconds left! Enter a solution for: %s\n%!"
      time_left combos
  else ()

(* Helper function for timer to do action [noti_time] whenever [incr]
   seconds have passed.

   Requires: incr > 0, time_limit > 0*)
let rec incr_timer (incr : int) combos (time_left : int) =
  Lwt.bind
    (incr |> float_of_int |> Lwt_unix.sleep)
    (fun () ->
      noti_time time_left combos;
      incr_timer incr combos (time_left - incr))

(* Main timer function. Calls [incr_timer] to repeatedly do an action
   whenever a certain amount of time has passed.

   Requires: incr > 0, time_limit > 0*)
let timer ?(incr : int = 1) ?(time_limit : int = 5) (combos : string) =
  let repeated_timer = incr_timer incr combos (time_limit - 1) in

  noti_time time_limit combos;
  Lwt.bind
    (time_limit |> float_of_int |> Lwt_unix.sleep)
    (fun () ->
      Lwt.cancel repeated_timer;
      print_endline "Wrong!";
      Lwt.return ())
