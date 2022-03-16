open Game.Valid_solution_checker
open Game.Timer

(** [combo_to_list str] converts [str] into an int list.

    Precondition - str is a string that only contains four positive
    integers separated by one space each. *)
let combo_to_list str =
  String.split_on_char ' ' str
  |> List.map (fun num -> int_of_string num)

(** [retrieve_combo str arr] retrieves a random element from [arr] if
    [str] is an empty string. Else it gives back [str]. *)
let retrieve_combo str arr =
  if str = "" then begin
    Random.self_init ();
    let combo_key = Array.length arr |> Random.int in
    Array.get arr combo_key
  end
  else str

(** [get_combination in_channel] creates an array with each element
    being a line from [in_channel]. *)
let rec get_combination in_channel =
  try
    let str = input_line in_channel in
    get_combination in_channel |> Array.append (Array.make 1 str)
  with End_of_file ->
    close_in_noerr in_channel;
    Array.make 0 ""

open Lwt
(** [play_game combo_array comb] handles the user inputs to actually
    play the game. *)

let create_timer line score =
  timer line (fun () ->
      Lwt_io.printf
        "\nTimes up. Thanks for playing! Final score: %N\n%!" score
      |> ignore;
      exit 0)

let rec play_game combo_array score comb =
  let line = retrieve_combo comb combo_array in
  Lwt_io.printf "Current Score: %N\n%!" score |> ignore;
  let time_counter, repeated_timer =
    timer line (fun () ->
        Lwt_io.printf
          "\nTimes up. Thanks for playing! Final score: %N\n%!" score
        |> ignore;
        exit 0)
  in
  let rec enter_sol
      ~time_counter
      ~repeated_timer
      ~line
      ~combo_array
      ~score =
    Lwt_io.printf "Enter solution for: %s\n>%!" line |> ignore;
    Lwt_io.read_line Lwt_io.stdin >>= function
    | "quit" -> Lwt_io.printl "Thank you for playing!"
    | ans -> begin
        match check_solution ans (combo_to_list line) with
        | Correct ->
            Lwt_io.printl "\nNice Job! Here's another one\n" |> ignore;
            cancel time_counter;
            cancel repeated_timer;
            play_game combo_array (score + 1) ""
        | Incorrect ->
            Lwt_io.printl "\nIncorrect, but nice attempt! Try again!\n"
            |> ignore;
            enter_sol ~time_counter ~repeated_timer ~line ~combo_array
              ~score
        | Invalid ->
            Lwt_io.printl
              "\nInvalid input, but nice attempt! Try again!\n"
            |> ignore;
            enter_sol ~time_counter ~repeated_timer ~line ~combo_array
              ~score
      end
  in
  enter_sol ~time_counter ~repeated_timer ~line ~combo_array ~score

(** [main ()] prompts the user to play the solo game, then starts it. *)
let main () =
  ANSITerminal.print_string [ ANSITerminal.red ]
    "\n\n\
     Welcome to 24 Battle Royale: Solo Edition! Given four numbers, \
     use addition, subtraction, multiplication, and division to make \
     24. Press enter to start the game or type \"quit\" to exit.\n";
  match read_line () with
  | "quit" | "\"quit\"" ->
      print_endline
        "Thank you for trying out 24 Battle Royale: Solo Edition!"
  | _ ->
      let in_channel =
        open_in ("assets" ^ Filename.dir_sep ^ "combos.txt")
      in
      play_game (get_combination in_channel) 0 ""
      |> Lwt_main.run |> ignore

(* Execute the game engine. *)
let () = main ()