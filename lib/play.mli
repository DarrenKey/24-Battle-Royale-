(** Module to manage the interactions between the user and the
    combinations. *)

val combo_to_list : string -> int list
(** [combo_to_list str] converts [str] into an int list.

    Precondition - str is a string that only contains four positive
    integers separated by one space each. *)

val retrieve_combo : string -> string array -> string
(** [retrieve_combo str arr] retrieves a random element from [arr] if
    [str] is an empty string. Else it gives back [str]. *)

val get_combination : in_channel -> string array
(** [get_combination in_channel] creates an array with each element
    being a line from [in_channel]. *)

val help_menu : string
(** [help_menu] is a string representing the game's help menu. *)

val make_two_sized : string -> string
(** [make_two_sized str] returns a string of length two if the length of
    [str] is two or one. *)

val line_array : string -> string array
(** [line_array line] returns an array representation of [line] if
    [line] is a string of four integers, with length < 3, with exactly
    one space in between each integer. *)

val nums_to_cards : string -> string
(** [nums_to_cards line] is a card visualization of the four numbers in
    line.

    Requires: line is a string with four integers 1 to 13 with one space
    in between each number. *)

val timer_reset : 'a Lwt.t -> 'b Lwt.t -> unit
(** [timer_reset] resets the timer for a problem. *)
