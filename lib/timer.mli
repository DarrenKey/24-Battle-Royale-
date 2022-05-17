(** Module to manage time related functions. *)

val incr_timer : int -> 'a -> int ref -> 'b Lwt.t
(** [incr_timer incr combos time_left] for timer to increment whenever
    [incr] seconds have passed. Requires: [incr] > 0, [time_left] > 0*)

val time_limit : int ref
(** [time_limit] refers to the time left. *)

val timer :
  ?incr:int -> string -> (unit -> unit) -> unit Lwt.t * 'a Lwt.t
(** [timer incr combos func_to_run] calls [incr_timer] on all the
    arguments to repeatedly do an action whenever a certain amount of
    time has passed.

    Requires: [incr] > 0, [time_limit] > 0*)

val time_left : int -> int -> unit -> int
(** [time_left starting_time total_time ()] is the time the user has
    remaining. *)

val game_over : int -> int -> bool
(** [game_over starting_time total_time] is true if
    [time_left starting_time total_time ()] is less than or equal to 0,
    else it is false. *)
