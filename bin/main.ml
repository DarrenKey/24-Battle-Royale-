(** Protocol for sending messages to the client: Sending something to
    the client involves two messages: (1st message): Command

    Valid Commands are: 'Msg' -> Send a message next line

    'Quit' -> Invokes a command to quit, with a quitting message after
    the line

    'Alert' -> Sends an alert next line, such as a "You are the host" message

    'Combos' -> Sends a set of 4 numbers the next line

    'Score' -> Sends the player'scurrent score

    'Time' -> Sends the player's current time

    "Num_in_lobby" -> Sends the number of people in the lobby currently

    [2nd message] : Included message, either from "Msg" or "Quit" *)

type client_command =
  | Alert
  | Combos
  | Score
  | Time
  | Num_in_lobby
  | Msg
  | Quit

let string_of_client client =
  client |> Ws.Client.id |> Ws.Client.Id.to_int |> string_of_int

let server = Ws.Server.create ~port:5000
let get_client_id client = client |> Ws.Client.id |> Ws.Client.Id.to_int
let check_game_status = ref false

let on_connect
    client_set
    host_id
    client_states
    lobbies
    client
    send_client_message =
  if !check_game_status then begin
    send_client_message
      "Game has already started, please wait for the current game to \
       end."
      Quit
    |> ignore;
    Ws.Server.close server client |> ignore
  end
  else begin
    if Hashtbl.length client_set = 0 then
      (host_id := get_client_id client;
       send_client_message
         "You're the host! Type /start to start the game.")
        Alert
      |> ignore
    else
      send_client_message "Waiting for the host to start the game..."
        Alert
      |> ignore;
    Hashtbl.add client_set (get_client_id client) client;
    let assigned_lobby = 0 in
    let client_id = get_client_id client in
    Hashtbl.add client_states client_id
      (assigned_lobby, [||], -1, "__INIT_COMBO", -1);
    print_endline
      ("added client " ^ string_of_int client_id ^ " to client_states");
    let clients_in_lobby, start_time =
      match Hashtbl.find_opt lobbies assigned_lobby with
      | Some (l, t) -> (l, t)
      | None -> ([], -1)
    in
    Hashtbl.replace lobbies assigned_lobby
      (client :: clients_in_lobby, start_time)
  end;
  Lwt.return ()

let rec next_host client_set host_id : Ws.Client.t =
  let pos_host_id = host_id + 1 in
  match Hashtbl.find_opt client_set pos_host_id with
  | Some client -> client
  | None -> next_host client_set pos_host_id

(* Broadcasts a message according to our new protocol. Takes in a
   message and command, sends command first then the message. *)

let broadcast_message server message (command : client_command) =
  let command =
    match command with
    | Msg -> "Msg"
    | Quit -> "Quit"
  in
  let%lwt command_sent = Ws.Server.broadcast server command in
  let%lwt message_sent = Ws.Server.broadcast server message in
  Lwt.return ()

(* Send a message according to our new protocol. Takes in a message and
   command, sends command first then the message. *)
let send_message_to_client client message (command : client_command) =
  let command =
    match command with
    | Alert -> "Alert"
    | Msg -> "Msg"
    | Quit -> "Quit"
    | Combos -> "Combos"
    | Score -> "Score"
    | Time -> "Time"
    | Num_in_lobby -> "Num_in_lobby"
  in
  let (p : bool Lwt.t), (r : bool Lwt.u) = Lwt.wait () in
  let%lwt command_sent = Ws.Client.send client command in
  let%lwt message_sent = Ws.Client.send client message in
  Lwt.wakeup r true;
  p

let rec run_start_setup server client_set client broadcast_message =
  failwith "run_start_setup deprecated"

let get_lobby lobbies lobby_id =
  match Hashtbl.find_opt lobbies lobby_id with
  | Some (clients, t) -> (clients, t)
  | None ->
      print_endline "Lobby not found: broadcast_message_to_lobby";
      failwith ""

let broadcast_message_to_lobby
    lobbies
    lobby_id
    message
    (command : client_command) =
  let lobby, _ = get_lobby lobbies lobby_id in
  List.fold_left
    (fun acc c -> send_message_to_client c message command |> ignore)
    () lobby
  |> ignore;
  Lwt.return ()

let get_client_lobby_id client_states client_id =
  match Hashtbl.find_opt client_states client_id with
  | Some (n, _, _, _, _) -> n
  | None ->
      print_endline "Client not found: get_client_lobby_id";
      failwith ""

let get_starting_time client_states client_id lobbies =
  let lobby_id = get_client_lobby_id client_states client_id in
  match Hashtbl.find_opt lobbies lobby_id with
  | Some (_, t) -> t
  | None ->
      print_endline "Lobby not found: get_starting_time";
      failwith ""

let get_client_state client_states client_id =
  match Hashtbl.find_opt client_states client_id with
  | Some (a, b, c, d, e) -> (a, b, c, d, e)
  | None ->
      print_endline "Client not found: get_client_state";
      failwith ""

(* This function should somehow be run asynchronously, at every tick,
   after the game starts *)
let rec game_loop lobby_id lobbies client_set client_states () =
  let loop = game_loop lobby_id lobbies client_set client_states in
  let rec update_clients lobby =
    match lobby with
    | [] -> []
    | c :: tail ->
        let client_id = get_client_id c in
        let send_client_message = send_message_to_client c in
        let starting_time =
          get_starting_time client_states client_id lobbies
        in
        let _, _, _, _, total_time =
          get_client_state client_states client_id
        in
        if Game.Timer.game_over starting_time total_time then (
          print_endline (string_of_int @@ Hashtbl.length client_set);
          Hashtbl.remove client_set client_id;
          print_endline @@ "checks " ^ string_of_int client_id;
          (let%lwt _ = send_client_message "Thanks for playing!" Quit in
           print_endline @@ "sends message to "
           ^ string_of_int client_id;
           (* somehow closing client 2 affected sending messages to
              client 1?? *)
           Ws.Server.close server c)
          |> ignore;
          print_endline @@ "quits " ^ string_of_int client_id;
          update_clients tail)
        else c :: update_clients tail
  in
  let clients, starting_time = get_lobby lobbies lobby_id in
  if List.length clients <= 1 then (
    check_game_status := false;
    Thread.exit ())
  else
    let updated_clients = update_clients clients in
    Hashtbl.replace lobbies lobby_id (updated_clients, starting_time);
    Thread.delay 0.2;
    loop ();
    ()

(* TODO: make this function to handle user input only instead *)
let run_game lobby_id client_set client_states lobbies client message =
  let open Game.Play in
  let open Game.Combinations in
  let open Game.Valid_solution_checker in
  let _ = print_endline "run_game run" in
  let client_id = get_client_id client in
  let print_client msg = send_message_to_client client msg Msg in
  let starting_time =
    get_starting_time client_states client_id lobbies
  in
  let combo_array, score, comb, total_time =
    match Hashtbl.find_opt client_states client_id with
    | Some (_, ca, s, c, t) -> (ca, s, c, t)
    | None ->
        print_endline "Client not found: run_game";
        failwith ""
  in
  let time_left = Game.Timer.time_left starting_time total_time in
  let broadcast_client_message =
    broadcast_message_to_lobby lobbies lobby_id
  in
  match String.split_on_char ' ' message with
  | [ "time" ] ->
      (* this statement matches for 2 but not for 1 *)
      print_endline "matching on time";
      let time_left = time_left () |> string_of_int in
      let _ = print_endline time_left in
      let%lwt a = time_left ^ " seconds left!" |> print_client in
      Lwt.return ()
  | "msg" :: other_msg ->
      broadcast_client_message
        ("User " ^ string_of_client client ^ ": "
        ^ String.concat " " other_msg)
        Msg
  | [ "repeat" ] ->
      print_client @@ "Enter solution for: " ^ comb ^ nums_to_cards comb
      |> ignore;
      Lwt.return ()
  | [ "skip" ] ->
      let lobby_id, combo_array, score, comb, total_game_time =
        Hashtbl.find client_states client_id
      in
      print_client @@ "Nice Attempt! Here's the solution: "
      ^ (String.split_on_char ' ' comb
        |> List.map int_of_string |> Combinations.solution_to)
      |> ignore;
      let new_score = if score - 1 > 0 then score - 1 else 0 in
      let new_line = retrieve_combo "" combo_array in
      print_client @@ "Current Score: " ^ string_of_int new_score
      ^ "\nEnter solution for: " ^ new_line ^ nums_to_cards new_line
      |> ignore;
      Hashtbl.replace client_states client_id
        (lobby_id, combo_array, new_score, new_line, total_game_time + 5)
      |> ignore;
      Lwt.return ()
  | [ "score" ] ->
      print_client @@ "Current Score: " ^ string_of_int score |> ignore;
      Lwt.return ()
  | x -> (
      let lobby_id, combo_array, score, comb, total_game_time =
        Hashtbl.find client_states client_id
      in
      if comb = "" then (
        let line = retrieve_combo comb combo_array in
        print_client @@ "Enter solution for: " ^ line
        ^ nums_to_cards line
        |> ignore;
        Hashtbl.replace client_states client_id
          (lobby_id, combo_array, score, line, total_game_time)
        |> ignore;
        Lwt.return ())
      else
        let ans = List.fold_left (fun acc x -> acc ^ " " ^ x) "" x in
        match check_solution ans (combo_to_list comb) with
        | Correct ->
            let new_line = retrieve_combo "" combo_array in
            print_client @@ "Nice Job! Here's another one: " ^ new_line
            ^ "\nCurrent Score: " ^ string_of_int (score + 1)
            ^ nums_to_cards new_line
            |> ignore;
            Hashtbl.replace client_states client_id
              ( lobby_id,
                combo_array,
                score + 1,
                new_line,
                total_game_time + 5 )
            |> ignore;
            Lwt.return ()
        | Incorrect ->
            print_client @@ "Incorrect, but nice attempt! Try again!"
            |> ignore;
            Lwt.return ()
        | Invalid ->
            print_client
            @@ "Invalid input, but nice attempt! Try again!"
            |> ignore;
            Lwt.return ())

(* In this context [message] refers to the message sent by client *)
let rec handler client_set host_id client_states lobbies client message
    =
  let _ = print_endline "handler run" in
  (let send_client_message = send_message_to_client client in
   match String.split_on_char ' ' message with
   | [ "on_connect" ] ->
       on_connect client_set host_id client_states lobbies client
         send_client_message
   | [ "debug" ] ->
       let client_id = get_client_id client in
       let lobby_id, combo_array, score, comb, total_game_time =
         get_client_state client_states client_id
       in
       let lobby, starting_time = get_lobby lobbies lobby_id in
       print_endline ("lobby_id: " ^ string_of_int lobby_id);
       print_endline
         ("lobby clients: "
         ^ List.fold_left
             (fun acc x -> acc ^ string_of_int (get_client_id x) ^ ", ")
             "" lobby);
       print_endline ("starting_time: " ^ string_of_int starting_time);
       print_endline ("client_id: " ^ string_of_int client_id);
       print_endline
         ("combo_array: "
         ^ Array.fold_left (fun acc x -> acc ^ x ^ ", ") "" combo_array
         );
       print_endline ("score: " ^ string_of_int score);
       print_endline ("comb: " ^ comb);
       print_endline
         ("total_game_time: " ^ string_of_int total_game_time);
       print_endline "";
       Lwt.return ()
   | _ -> Lwt.return ())
  |> ignore;
  let client_id = get_client_id client in
  let client_lobby_id = get_client_lobby_id client_states client_id in
  if !check_game_status then
    run_game client_lobby_id client_set client_states lobbies client
      message
  else
    run_lobby client_lobby_id client_set host_id client_states lobbies
      client message

and run_lobby
    lobby_id
    client_set
    host_id
    client_states
    lobbies
    client
    message =
  let _ = print_endline "run_lobby run" in
  let client_id = get_client_id client in
  let lobby, _ = get_lobby lobbies lobby_id in
  let send_client_message = send_message_to_client client in
  match String.split_on_char ' ' message with
  | [ "debug" ] -> Lwt.return ()
  | [ "on_connect" ] -> Lwt.return ()
  | [ "debug_host" ] ->
      let%lwt _ = send_client_message (!host_id |> string_of_int) Msg in
      Lwt.return ()
  | [ "/start" ] ->
      if !host_id = client_id then (
        check_game_status := true;
        ignore (broadcast_message server "Game starting!" Msg);
        Hashtbl.replace lobbies lobby_id
          (lobby, int_of_float @@ Unix.time ());
        (let in_channel =
           open_in ("assets" ^ Filename.dir_sep ^ "combos.txt")
         in
         let combos = Game.Play.get_combination in_channel in
         let _ = print_endline "got combos" in
         (* initialize every client in the current lobby *)
         List.fold_left
           (fun acc c ->
             Hashtbl.replace client_states (get_client_id c)
               (lobby_id, combos, 0, "", 30))
           () lobby);
        Thread.create
          (game_loop lobby_id lobbies client_set client_states)
          ()
        |> ignore;
        run_game lobby_id client_set client_states lobbies client
          message)
      else
        let%lwt _ = send_client_message "You are not the host!" Msg in
        Lwt.return ()
  | [ "/quit" ] ->
      Hashtbl.remove client_set client_id;
      if
        get_client_id client = !host_id
        && Hashtbl.length client_set >= 1
      then begin
        let%lwt _ = send_client_message "Quit successfully" Quit in
        Ws.Server.close server client |> ignore;
        let new_host = next_host client_set !host_id in
        host_id := get_client_id new_host;
        let%lwt _ =
          send_message_to_client new_host "You are now the new host!"
            Msg
        in
        Lwt.return ()
      end
      else if
        get_client_id client = !host_id && Hashtbl.length client_set = 0
      then begin
        check_game_status := false;
        let%lwt _ = send_client_message "Quit successfully" Quit in
        Ws.Server.close server client
      end
      else
        let%lwt _ = send_client_message "Quit successfully" Quit in
        Ws.Server.close server client
  | [ a; b; c; d; e ] when a = "evaluate" ->
      let%lwt _ =
        send_client_message
          (Game.Combinations.Combinations.solution_to
             (List.map int_of_string [ b; c; d; e ]))
          Msg
      in
      Lwt.return ()
  (* Parrots message ot everyone. Kept in for now as a debug measure *)
  | "msg" :: other_msg ->
    broadcast_message server 
        ("User " ^ string_of_client client ^ ": "
        ^ String.concat " " other_msg)
        Msg
  | _ -> Lwt_io.printl "pattern match failed!"

let () =
  let client_set : (int, Ws.Client.t) Hashtbl.t = Hashtbl.create 50 in
  (* maps client ids to (lobby_id, combo_array, score, comb,
     total_game_time) *)
  let client_states :
      (int, int * string array * int * string * int) Hashtbl.t =
    Hashtbl.create 50
  in
  (* TODO: make each lobby have a different host! *)
  (* the float represents the time at which the lobby started*)
  let lobbies : (int, Ws.Client.t list * int) Hashtbl.t =
    Hashtbl.create 50
  in
  let host_id = ref 0 in
  let helper_connect temp = Lwt.return () in
  Lwt_main.run
    (Ws.Server.run server (Some helper_connect)
       (handler client_set host_id client_states lobbies))
