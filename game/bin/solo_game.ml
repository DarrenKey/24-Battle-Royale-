open Game.Valid_solution_checker

(** [combo_to_list str] converts [str] into an int list.

    Precondiiton - str is a string that only contains four positive
    integers separated by one space each. *)
let combo_to_list str =
  String.split_on_char ' ' str
  |> List.map (fun num -> int_of_string num)

(** [retrieve_line str in_channel] retrieves a new input string from
    [in_channel] if there currently is none stored in [str]. *)
let retrieve_line str in_channel =
  if str = "" then input_line in_channel else str

(** [play_game in_channel comb] handles the user inputs to actually play
    the game. *)
let rec play_game in_channel comb =
  try
    let line = retrieve_line comb in_channel in
    print_endline ("Enter solution for: " ^ line);
    print_string "> ";
    match read_line () with
    | "quit" ->
        flush stdout;
        close_in in_channel;
        print_endline "Thank you for playing!"
    | ans -> begin
        match check_solution ans (combo_to_list line) with
        | Correct ->
            print_endline "";
            print_endline "Nice Job! Here's another one";
            play_game in_channel ""
        | Incorrect ->
            print_endline "";
            print_endline "Incorrect, but nice attempt! Try again!";
            play_game in_channel line
        | Invalid ->
            print_endline "";
            print_endline "Invalid input, but nice attempt! Try again!";
            play_game in_channel line
      end
  with error ->
    close_in_noerr in_channel;
    print_endline "Unknown error has occured. Thank you for playing!"

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
      play_game in_channel ""

(* Execute the game engine. *)
let () = main ()