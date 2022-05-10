let combo_to_list str =
  String.split_on_char ' ' str
  |> List.map (fun num -> int_of_string num)

let retrieve_combo str arr =
  if str = "" then begin
    Random.self_init ();
    let combo_key = Array.length arr |> Random.int in
    Array.get arr combo_key
  end
  else str

let rec get_combination in_channel =
  try
    let str = input_line in_channel in
    get_combination in_channel |> Array.append (Array.make 1 str)
  with End_of_file ->
    close_in_noerr in_channel;
    Array.make 0 ""

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
   \"quit\": Exits the game\n\
   \"skip\": Skips the current question and gives a new one for a \
   score penalty\n\
   \"time\": Displays the time left for the current problem\n\
   \"repeat\": Repeats the current problem\n\
   \"help\": Opens up the help menu\n\n\
   Hope this helped! Good luck!\n"

let make_two_sized str =
  if String.length str < 2 then " " ^ str else str

let line_array line =
  let num_list = String.split_on_char ' ' line in
  let rec line_to_array num_list =
    match num_list with
    | h :: t ->
        line_to_array t
        |> Array.append (make_two_sized h |> Array.make 1)
    | [] -> Array.make 0 ""
  in
  line_to_array num_list

let nums_to_cards line =
  let combo_array = line_array line in
  "\n.--------. .--------. .--------. .--------.\n|"
  ^ Array.get combo_array 0 ^ ".--.  | |" ^ Array.get combo_array 1
  ^ ".--.  | |" ^ Array.get combo_array 2 ^ ".--.  | |"
  ^ Array.get combo_array 3
  ^ ".--.  |\n\
     |  :/\\:  | |  (\\/)  | |  :():  | |  :/\\:  |\n\
     |  (__)  | |  :\\/:  | |  ()()  | |  :\\/:  |\n\
     |  '--'" ^ Array.get combo_array 0 ^ "| |  '--'"
  ^ Array.get combo_array 1 ^ "| |  '--'" ^ Array.get combo_array 2
  ^ "| |  '--'" ^ Array.get combo_array 3
  ^ "|\n`--------' `--------' `--------' `--------'"

let timer_reset time_counter repeated_timer =
  Lwt.cancel time_counter;
  Lwt.cancel repeated_timer;
  Timer.time_limit := 40
