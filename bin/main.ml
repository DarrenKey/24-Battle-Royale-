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

let on_connect client_set host_id client send_client_message =
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
  Lwt.return ()

let run_start_setup server client broadcast_message =
  check_game_status := true;
  broadcast_message "Game starting!" Msg

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

let handler client_set host_id client message =
  let client_id = get_client_id client in
  let send_client_message = send_message_to_client client in
  let broadcast_client_message = broadcast_message server in
  match String.split_on_char ' ' message with
  | [ "debug_host" ] ->
      send_client_message (!host_id |> string_of_int) Msg
  | [ "on_connect" ] ->
      on_connect client_set host_id client send_client_message
  | [ "/start" ] ->
      if !host_id = client_id then
        run_start_setup server client broadcast_client_message
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

let () =
  let client_set : (int, Ws.Client.t) Hashtbl.t = Hashtbl.create 50 in
  let host_id = ref 0 in
  let helper_connect temp = Lwt.return () in
  Lwt_main.run
    (Ws.Server.run server (Some helper_connect)
       (handler client_set host_id))
