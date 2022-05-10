(** Protocol for sending messages to the client: Sending something to
    the client involves two messages: (1st message): Command

    Valid Commands are: 'Msg' -> Send a message next line

    'Quit' -> Invokes a command to quit, with a quitting message after
    the line

    [2nd message] : Included message, either from "Msg" or "Quit" *)

type client_command =
  | Msg
  | Quit

let string_of_client client =
  client |> Ws.Client.id |> Ws.Client.Id.to_int |> string_of_int

let server = Ws.Server.create ~port:3000
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
        Msg
      |> ignore
    else
      send_client_message "Waiting for the host to start the game..."
        Msg
      |> ignore;
    Hashtbl.add client_set (get_client_id client) client
  end;
  let assigned_lobby = 0 in
  let client_id = get_client_id client in
  Hashtbl.add client_states client_id
    (assigned_lobby, [||], -1, "__INIT");
  print_endline
    ("added client " ^ string_of_int client_id ^ " to client_states");
  let lobby =
    match Hashtbl.find_opt lobbies assigned_lobby with
    | Some l -> l
    | None -> []
  in
  Hashtbl.replace lobbies assigned_lobby (client :: lobby);
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
    | Msg -> "Msg"
    | Quit -> "Quit"
  in
  let%lwt command_sent = Ws.Client.send client command in
  let%lwt message_sent = Ws.Client.send client message in
  Lwt.return ()

let rec run_start_setup server client_set client broadcast_message =
  failwith "run_start_setup deprecated"

let get_lobby lobbies lobby_id =
  match Hashtbl.find_opt lobbies lobby_id with
  | Some clients -> clients
  | None ->
      print_endline "Lobby not found: broadcast_message_to_lobby";
      failwith ""

let broadcast_message_to_lobby
    lobbies
    lobby_id
    message
    (command : client_command) =
  let lobby = get_lobby lobbies lobby_id in
  List.map (fun c -> send_message_to_client c message command) lobby
  |> ignore;
  Lwt.return ()

(* In this context [message] refers to the message sent by client *)
let rec handler client_set host_id client_states lobbies client message
    =
  (let send_client_message = send_message_to_client client in
   match String.split_on_char ' ' message with
   | [ "on_connect" ] ->
       on_connect client_set host_id client_states lobbies client
         send_client_message
   | _ -> Lwt.return ())
  |> ignore;
  let client_id = get_client_id client in
  let client_lobby_id =
    match Hashtbl.find_opt client_states client_id with
    | Some (n, _, _, _) -> n
    | None ->
        print_endline "Client not found: handler";
        failwith ""
  in
  if !check_game_status = true then
    run_game client_set client_states client message
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
  let client_id = get_client_id client in
  let lobby = get_lobby lobbies lobby_id in
  let send_client_message = send_message_to_client client in
  let broadcast_client_message =
    broadcast_message_to_lobby lobbies lobby_id
  in
  match String.split_on_char ' ' message with
  | [ "on_connect" ] -> Lwt.return ()
  | [ "debug_host" ] ->
      send_client_message (!host_id |> string_of_int) Msg
  | [ "/start" ] ->
      if !host_id = client_id then (
        check_game_status := true;
        ignore (broadcast_message server "Game starting!" Msg);
        let in_channel =
          open_in ("assets" ^ Filename.dir_sep ^ "combos.txt")
        in
        List.map
          (fun c ->
            match Hashtbl.find_opt client_states client_id with
            | Some (ln, _, _, _) ->
                Hashtbl.replace client_states client_id
                  (ln, Game.Play.get_combination in_channel, 0, "")
            | None ->
                print_endline "Client not found: run_lobby";
                failwith "")
          lobby
        |> ignore;
        run_game client_set client_states client message)
      else send_client_message "You are not the host!" Msg
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
        send_message_to_client new_host "You are now the new host!" Msg
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
      send_client_message
        (Game.Combinations.Combinations.solution_to
           (List.map int_of_string [ b; c; d; e ]))
        Msg
  (* Parrots message ot everyone. Kept in for now as a debug measure *)
  | other_msg ->
      broadcast_client_message
        ("User " ^ string_of_client client ^ ": "
        ^ String.concat " " other_msg)
        Msg

and run_game client_set client_states client message =
  let client_id = get_client_id client in
  let send_client_message = send_message_to_client client in
  let combo_array, score, comb =
    match Hashtbl.find_opt client_states client_id with
    | Some (_, ca, s, c) -> (ca, s, c)
    | None ->
        print_endline "Client not found: run_game";
        failwith ""
  in
  let open Game.Play in
  let open Game.Timer in
  let open Game.Combinations in
  let open Game.Valid_solution_checker in
  let line = retrieve_combo comb combo_array in
  send_client_message ("Current Score: " ^ string_of_int score) Msg
  |> ignore;
  let time_counter, repeated_timer =
    timer line (fun () ->
        Hashtbl.remove client_set client_id;
        send_client_message
          ("\nTimes up! Correct answer for " ^ line ^ " is: "
          ^ (String.split_on_char ' ' line
            |> List.map int_of_string |> Combinations.solution_to))
          Msg
        |> ignore;
        send_client_message
          ("Thanks for playing!\nFinal score: " ^ string_of_int score)
          Quit
        |> ignore;
        Ws.Server.close server client |> ignore)
  in
  let rec enter_sol
      ~time_counter
      ~repeated_timer
      ~line
      ~combo_array
      ~score =
    send_client_message
      ("Enter solution for: " ^ line ^ nums_to_cards line)
      Msg
    |> ignore;
    match message with
    | "time" | "\"time\"" ->
        send_client_message "\n" Msg |> ignore;
        let time_left = !Game.Timer.time_limit |> string_of_int in
        send_client_message (time_left ^ " seconds left!") Msg |> ignore;
        enter_sol ~time_counter ~repeated_timer ~line ~combo_array
          ~score
    | "repeat" | "\"repeat\"" ->
        send_client_message "\n" |> ignore;
        enter_sol ~time_counter ~repeated_timer ~line ~combo_array
          ~score
    | "quit" | "\"quit\"" ->
        send_client_message "Thank you for playing!" Quit
    | ans -> begin
        match check_solution ans (combo_to_list line) with
        | Correct ->
            send_client_message "\nNice Job! Here's another one\n" Msg
            |> ignore;
            timer_reset time_counter repeated_timer;

            run_game combo_array (score + 1) ""
        | Incorrect ->
            send_client_message
              "\nIncorrect, but nice attempt! Try again!\n" Msg
            |> ignore;
            enter_sol ~time_counter ~repeated_timer ~line ~combo_array
              ~score
        | Invalid ->
            send_client_message
              "\nInvalid input, but nice attempt! Try again!\n" Msg
            |> ignore;
            enter_sol ~time_counter ~repeated_timer ~line ~combo_array
              ~score
      end
  in

  Lwt.return
    (enter_sol ~time_counter ~repeated_timer ~line ~combo_array ~score)

let () =
  let client_set : (int, Ws.Client.t) Hashtbl.t = Hashtbl.create 50 in
  (* maps client ids to (lobby_id, combo_array, score, comb) *)
  let client_states : (int, int * string array * int * string) Hashtbl.t
      =
    Hashtbl.create 50
  in
  (* TODO: make each lobby have a different host! *)
  let lobbies : (int, Ws.Client.t list) Hashtbl.t = Hashtbl.create 50 in
  let host_id = ref 0 in
  let helper_connect temp = Lwt.return () in
  Lwt_main.run
    (Ws.Server.run server (Some helper_connect)
       (handler client_set host_id client_states lobbies))
