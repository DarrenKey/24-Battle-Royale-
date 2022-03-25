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

let help_menu =
  "\n\
   ~24 Battle Royale: Solo Edition Help Menu~ \n\n\
   You will be given 4 numbers. Use each of them exactly once to \
   create 24. When the timer hits 0, the game ends.\n\n\
   Valid arithmetic operators are:\n\
   Addition: + \n\
   Subtraction: - \n\
   Multiplication: x, * \n\
   Division: / \n\
   Parenthesis: ( ) [ ] \n\n\
   A valid expression is:\n\
   -Only uses the four numbers and valid operations\n\
   -Proper opening and closing parentheses\n\n\
   Valid non-expression commands are:\n\
   \"quit\": exits the game\n\
   \"help\": opens up the help menu\n\n\
   Hope this helped! Good luck!\n"

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
    | "quit" | "\"quit\"" -> Lwt_io.printl "Thank you for playing!"
    | "help" ->
        Lwt_io.printl help_menu |> ignore;
        enter_sol ~time_counter ~repeated_timer ~line ~combo_array
          ~score
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

(** [main_quit_and_enter input] checks if [input] is quit or something
    else and prints accordingly. Used as a helper function for
    [main ()]. *)
let main_quit_and_enter input =
  match input with
  | "quit" | "\"quit\"" ->
      print_endline
        "Thank you for trying out 24 Battle Royale: Solo Edition!"
  | _ ->
      let in_channel =
        open_in ("assets" ^ Filename.dir_sep ^ "combos.txt")
      in
      play_game (get_combination in_channel) 0 ""
      |> Lwt_main.run |> ignore

(** [main ()] prompts the user to play the solo game, then starts it. *)
let main () =
  ANSITerminal.print_string [ ANSITerminal.red ]
    "\n\n\
     Welcome to 24 Battle Royale: Solo Edition! Given four numbers, \
     use addition, subtraction, multiplication, and division to make \
     24. Don't let the timer reach 0 or else the game ends! Press \
     enter to start the game or type \"quit\" to exit. Type \"help\" \
     for the help menu.\n";
  match read_line () with
  | "help" | "\"help\"" ->
      print_endline help_menu;
      ANSITerminal.print_string [ ANSITerminal.red ]
        "Press enter to start the game. Or type \"quit\" to exit.\n";
      read_line () |> main_quit_and_enter
  | input -> main_quit_and_enter input

(* Execute the game engine. *)
let () = main ()