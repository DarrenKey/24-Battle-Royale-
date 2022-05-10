open Game.Valid_solution_checker
open Game.Timer
open Game.Combinations
open Game.Play
open Lwt

(** [play_game combo_array comb] handles the user inputs to actually
    play the game. *)
let rec play_game combo_array score comb =
  let line = retrieve_combo comb combo_array in
  Lwt_io.printf "Current Score: %N\n%!" score |> ignore;
  let time_counter, repeated_timer =
    timer line (fun () ->
        Lwt_io.printl
          ("\nTimes up! Correct answer for " ^ line ^ " is: "
          ^ (String.split_on_char ' ' line
            |> List.map int_of_string |> Combinations.solution_to))
        |> ignore;
        Lwt_io.printf "Thanks for playing!\nFinal score: %N\n%!" score
        |> ignore;
        exit 0)
  in
  let rec enter_sol
      ~time_counter
      ~repeated_timer
      ~line
      ~combo_array
      ~score =
    Lwt_io.printf "Enter solution for: %s\n>%!"
      (line ^ nums_to_cards line)
    |> ignore;
    Lwt_io.read_line Lwt_io.stdin >>= function
    | "time" | "\"time\"" ->
        Lwt_io.printl "" |> ignore;
        let time_left = !Game.Timer.time_limit |> string_of_int in
        time_left ^ " seconds left!" |> Lwt_io.printl |> ignore;
        Lwt_io.printl "" |> ignore;
        enter_sol ~time_counter ~repeated_timer ~line ~combo_array
          ~score
    | "repeat" | "\"repeat\"" ->
        Lwt_io.printl "" |> ignore;
        enter_sol ~time_counter ~repeated_timer ~line ~combo_array
          ~score
    | "quit" | "\"quit\"" -> Lwt_io.printl "Thank you for playing!"
    | "panda" ->
        Lwt_io.printl
          "    ████████                                          \n\
          \  ████    ████░░              ░░                      \n\
           ██▓▓██    ▓▓████    ████████████████                  \n\
           ▓▓██        ████████                ██████    ████    \n\
           ████    ██████                          ████████████  \n\
           ████  ████                              ██████  ██████\n\
          \  ██████                                ████      ████\n\
          \  ████                                    ████      ██\n\
          \  ██                                        ████  ████\n\
           ██                                            ████████\n\
           ██                                              ████  \n\
           ██                                              ██    \n\
           ██        ░░                                      ██  \n\
           ▓▓                                                ██  \n\
           ▓▓                                                ▓▓  \n\
           ██    ██████                        ██████        ██  \n\
           ██  ██    ████                    ██    ████      ██  \n\
           ██  ████████▓▓                    ████▓▓████      ██  \n\
           ██  ▓▓████████                    ██████████      ██  \n\
           ██░░░░████▓▓                        ██████░░░░░░  ██  \n\
          \  ██░░░░░░░░░░  ██    ██    ██        ░░░░░░░░░░██    \n\
          \  ██░░░░░░░░░░    ████  ████    ░░    ░░░░░░░░░░██    \n\
          \    ██░░░░░░                          ░░░░░░░░██      \n\
          \      ██░░░░                          ░░░░░░██        \n\
          \        ▓▓██                            ████          \n\
          \    ██████████████████▓▓████████████████████          \n\
          \    ██████▓▓▓▓▓▓                  ▓▓██▓▓▓▓██          \n\
          \    ██████▓▓▓▓▓▓                ████████████          \n\
          \    ████████▓▓██                ████████████          \n\
          \    ████████▓▓██                ████████████          \n\
          \      ▓▓██▓▓██                  ████▓▓██████          \n\
          \      ██                          ████▓▓██            \n\
          \      ██                                ██            \n\
          \      ██              ░░      ░░        ██            \n\
          \      ██                                ██            \n\
          \      ██                                ████          \n\
          \        ██                            ██████          \n\
          \        ██  ████████        ████████  ██████          \n\
          \          ████████████    ██████████▓▓████            \n\
          \          ██▓▓████████████▓▓██▓▓██████                \n\
          \            ████████        ████████                  \n"
        |> ignore;
        enter_sol ~time_counter ~repeated_timer ~line ~combo_array
          ~score
    | "camel" ->
        Lwt_io.printl
          "  ██████  ████████  ██████              \
           ██████████                          \n\
          \  ██░░████░░░░░░░░██░░░░██        \
           ██████░░░░░░░░▒▒██████                    \n\
          \    ▓▓░░░░░░░░░░░░░░██▓▓      \
           ▓▓▓▓░░░░░░░░░░░░▒▒▒▒░░░░▒▒▓▓                  \n\
          \    ████░░░░░░████░░██      \
           ██░░░░░░░░░░░░░░░░░░░░░░░░░░░░██                \n\
          \  ██░░      ░░░░░░░░▓▓    \
           ▓▓░░░░░░░░░░▒▒░░░░░░░░░░░░▒▒░░░░░░██              \n\
          \  ████      ▓▓  ░░░░▓▓  \
           ▒▒░░░░▒▒░░▒▒▒▒▒▒░░░░░░░░░░░░░░░░░░░░░░██            \n\
          \  ██  ██  ██    ░░░░▓▓  \
           ▓▓░░░░░░░░▒▒▒▒░░░░░░░░░░▒▒░░░░░░▒▒▒▒░░██            \n\
          \  ██            \
           ░░██░░██░░░░░░░░░░░░░░░░░░░░░░▒▒▒▒▒▒░░░░▒▒▒▒░░░░██          \n\
          \  ██▒▒      ▒▒  \
           ██▒▒░░██▒▒▒▒░░░░░░░░░░▒▒░░░░░░▒▒░░░░░░░░░░░░░░░░██          \n\
          \  ██  ▒▒▒▒▒▒    \
           ██░░░░██▒▒▒▒░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░██          \n\
          \    \
           ████████████░░░░░░██░░░░░░████░░░░░░▒▒▒▒░░░░░░░░░░▒▒░░░░░░████          \n\
          \          \
           ██░░░░░░░░░░██░░░░██░░░░██░░░░▒▒▒▒░░░░░░░░░░░░░░████▓▓▓▓██        \n\
          \        \
           ██░░░░░░░░░░░░██░░██▒▒████████░░░░░░░░░░░░░░░░████▒▒▒▒▓▓██░░▓▓      \n\
          \        \
           ██░░░░░░░░░░░░░░██▒▒██▒▒▒▒▒▒▒▒██░░░░░░░░██████▒▒▒▒▓▓▓▓▓▓░░░░██      \n\
          \        ██  \
           ░░░░░░░░░░░░██▒▒░░▒▒████▓▓░░██▓▓████▒▒▒▒▓▓▓▓██████░░░░░░░░▓▓    \n\
          \        \
           ██░░░░░░░░░░░░░░░░██░░░░░░░░░░░░██▒▒▒▒▓▓▓▓██████▒▒▒▒░░░░░░░░░░██    \n\
          \          \
           ██░░░░░░░░░░░░░░░░░░░░░░░░░░░░██████████░░░░░░░░░░░░░░▒▒░░▒▒▒▒██  \n\
          \          \
           ██░░░░░░░░░░░░░░░░░░░░░░░░░░░░██░░░░░░░░░░░░░░░░░░░░▒▒▒▒░░▒▒░░██  \n\
          \          \
           ██░░░░░░░░░░░░░░░░░░░░░░░░░░░░▒▒▓▓░░▒▒░░░░▒▒▒▒░░░░░░▒▒░░░░░░░░▒▒▓▓\n\
          \          \
           ██░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░██░░░░░░░░▒▒░░░░░░░░░░░░░░░░░░░░██\n\
          \          \
           ░░▓▓░░░░░░░░░░░░░░░░░░░░░░░░░░░░██░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░██\n\
          \            \
           ██░░░░░░░░░░░░░░░░░░░��░░░░░░░░██░░░░░░░░░░░░░░▒▒▒▒░░░░▒▒░░░░░░██\n\
          \            \
           ██░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░██░░░░▒▒░░░░░░▒▒▒▒▒▒░░░░░░░░██  \n\
          \              \
           ▓▓░░░░░░░░░░░░██░░░░░░░░░░░░░░██░░░░░░░░░░░░▒▒▒▒░░░░░░░░██    \n\
          \                \
           ▓▓▓▓▓▓░░░░░░▓▓░░░░░░░░░░░░░░██░░░░░░░░░░░░░░░░░░░░██████    \n\
          \                    \
           ▓▓░░░░░░░░██░░░░░░░░░░░░██░░░░░░░░░░░░░░░░████▒▒▒▒██    \n\
          \                      \
           ██░░░░░░██░░░░░░░░░░██░░████░░░░░░██████░░░░░░░░██    \n\
          \                      \
           ██░░░░░░░░██░░░░░░░░██████████████░░░░██░░░░░░██      \n\
          \                      ██░░░░░░░░██░░░░░░░░██      \
           ░░██░░░░░░██░░░░░░██      \n\
          \                      ██░░░░░░░░██░░░░░░░░██          \
           ██░░░░░░██░░░░██      \n\
          \                        ▓▓░░░░░░██░░░░░░▓▓░░          \
           ██░░░░░░██░░░░██      \n\
          \                        ██░░░░██  ██░░░░██            \
           ██░░░░░░██░░░░██      \n\
          \                        ██░░░░▓▓  ██░░░░██              \
           ██░░██  ▓▓░░██      \n\
          \                        ██░░░░▓▓  ██░░▓▓                \
           ██░░██  ▓▓░░▓▓      \n\
          \                        ██░░░░██  ▓▓░░██                \
           ██░░██  ██░░██      \n\
          \                      ██░░░░▓▓░░  ██░░░░██            \
           ██░░░░██  ██░░░░██    \n\
          \                      ██░░░░██    ██░░░░██            \
           ██░░░░██  ██░░░░██    \n\
          \                      ██░░░░██    ██░░░░██            \
           ██░░░░██  ██░░░░██    \n\
          \                        ██░░██    ██░░░░██              \
           ██░░██  ██░░░░██    \n\
          \                        ██░░██    ░░██░░██              \
           ██░░██    ▓▓░░██    \n\
          \                        ██░░██      ██░░██              \
           ██░░██    ██░░██    \n\
          \                      ██░░░░██      ██░░██            \
           ██░░░░██    ██░░██    \n\
          \                      ▓▓░░▓▓        ██░░██            \
           ▓▓░░▓▓      ██░░██    \n\
          \                      ▓▓░░▓▓        ██░░██            \
           ▓▓░░▓▓      ██░░██    \n\
          \                      ██░░██        ██░░██            \
           ██░░██      ██░░██    \n\
          \                    ██▒▒▓▓██      ██▒▒▓▓██          \
           ██▒▒▓▓██    ██▒▒▓▓██    \n\
          \                    ██████░░      ████▓▓██          \
           ████████    ████████    \n"
        |> ignore;
        enter_sol ~time_counter ~repeated_timer ~line ~combo_array
          ~score
    | "help" | "\"help\"" ->
        Lwt_io.printl help_menu |> ignore;
        enter_sol ~time_counter ~repeated_timer ~line ~combo_array
          ~score
    | "skip" | "\"skip\"" ->
        Lwt_io.printl "" |> ignore;
        Lwt_io.printl
          ("Nice Attempt! Here's the solution: "
          ^ (String.split_on_char ' ' line
            |> List.map int_of_string |> Combinations.solution_to))
        |> ignore;
        Lwt_io.printl "" |> ignore;
        timer_reset time_counter repeated_timer;
        play_game combo_array
          (if score - 1 > 0 then score - 1 else 0)
          ""
    | ans -> begin
        match check_solution ans (combo_to_list line) with
        | Correct ->
            Lwt_io.printl "\nNice Job! Here's another one\n" |> ignore;
            timer_reset time_counter repeated_timer;
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
  ANSITerminal.print_string [ ANSITerminal.green ]
    ".-.  .-.,---.  ,-.    ,--,  .---.           ,---.    _______  \
     .---.   \n\
     | |/\\| || .-'  | |  .' .') / .-. ) |\\    /| | .-'   |__   __|/ \
     .-. )  \n\
     | /  \\ || `-.  | |  |  |(_)| | |(_)|(\\  / | | `-.     )| |   | \
     | |(_) \n\
     |  /\\  || .-'  | |  \\  \\   | | | | (_)\\/  | | .-'    (_) |   \
     | | | |  \n\
     |(/  \\ ||  `--.| `--.\\  `-.\\ `-' / | \\  / | |  `--.    | |   \
     \\ `-' /  \n\
     (_)   \\|/( __.'|( __.'\\____\\)---'  | |\\/| | /( __.'    `-'    \
     )---'   \n\
    \       (__)    (_)         (_)     '-'  '-'(__)              \
     (_)      \n\n";
  ANSITerminal.print_string [ ANSITerminal.yellow ]
    "                         _____                                  \n\
    \       .-''-.           /    /                                  \n\
    \     .' .-.  )         /    /                /|                 \n\
    \    / .'  / /         /    /                 ||                 \n\
    \   (_/   / /         /    /                  ||        .-,.--.  \n\
    \        / /         /    /  __               ||  __    |  .-. | \n\
    \       / /         /    /  |  |              ||/'__ '. | |  | | \n\
    \      . '         /    '   |  |              |:/`  '. '| |  | | \n\
    \     / /    _.-')/    '----|  |---.          ||     | || |  '-  \n\
    \   .' '  _.'.-''/          |  |   |          ||\\    / '| |      \n\
    \  /  /.-'_.'    '----------|  |---'          |/\\'..' / | |      \n\
    \ /    _.'                  |  |              '  `'-'`  |_|      \n\
     ( _.-'                    /____\\                                ";
  ANSITerminal.print_string [ ANSITerminal.cyan ]
    "\n\n\
     Welcome to 24 Battle Royale: Solo Edition! Given four numbers, \
     use addition, subtraction, multiplication, and division to make \
     24. Feel free to use the \"skip\" command if you get stuck on a \
     question for a small penalty. Use the \"help\" command to view \
     the help menu. Press enter to start the game or type \"quit\" to \
     exit.\n";
  match read_line () with
  | "help" | "\"help\"" ->
      ANSITerminal.print_string [ ANSITerminal.magenta ] help_menu;
      ANSITerminal.print_string [ ANSITerminal.red ]
        "Press enter to start the game. Or type \"quit\" to exit.\n";
      read_line () |> main_quit_and_enter
  | input -> main_quit_and_enter input

(* Execute the game engine. *)
let () = main ()