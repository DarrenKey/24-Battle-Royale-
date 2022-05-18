(** Protocol for sending messages to the client: Sending something to
    the client involves sening first a command, then the message: (1st
    message)|Command

    Valid Commands are: 'Msg' -> Send a message next line

    'Quit' -> Invokes a command to quit with a quitting message

    'Alert' -> Sends an alert such as a "You are the host" message

    'Problem' -> Sends a set of 4 numbers the next line

    'Score' -> Sends the player'scurrent score

    'Time' -> Sends the player's current time

    "Num_in_lobby" -> Sends the number of people in the lobby currently *)

type client_command =
  | Alert
  | Problem
  | Score
  | Time
  | Num_in_lobby
  | Msg
  | Quit

(* Record of client information *)
type client_info = {
  combo_array : string array;
  score : int;
  combo : string;
  total_game_time : int;
}

let string_of_client client =
  client |> Ws.Client.id |> Ws.Client.Id.to_int |> string_of_int

let server = Ws.Server.create ~port:5000

(* Gets client_id as an int from the client. *)
let get_client_id client = client |> Ws.Client.id |> Ws.Client.Id.to_int

(* Checks if there exists more than 0 current clients. *)
let exists_clients () = server |> Ws.Server.current_connections > 0

(* Checks if there exists more than 1 current clients. *)
let exists_multiple () = server |> Ws.Server.current_connections > 1

(* Gets a client that's currently connected via the client_id. *)
let get_client_in_current_connections client_id =
  List.fold_right
    (fun client acc ->
      match acc with
      | Some x -> Some x
      | None ->
          if get_client_id client = client_id then Some client else None)
    (Ws.Server.clients server)
    None

(* Gets the oldest client that doesn't equal some client_id. *)
let get_oldest_client_not_equal client_id_to_avoid =
  let rec iter_client = function
    | [] -> failwith "No clients in current connection!"
    | [ x ] -> get_client_id x
    | h :: t when not (get_client_id h = client_id_to_avoid) ->
        min (get_client_id h) (iter_client t)
    | h :: t -> iter_client t
  in
  server |> Ws.Server.clients |> iter_client
  |> get_client_in_current_connections
  |> function
  | Some x -> x
  | None -> failwith "No clients in current connection!"

let check_game_status = ref false

(* Interval for checking the time. *)
let time_interval = 0.2

(* Converts a given command to the string name. For broadcasting +
   sending purposes. *)
let convert_command_to_string = function
  | Alert -> "Alert"
  | Msg -> "Msg"
  | Quit -> "Quit"
  | Problem -> "Problem"
  | Score -> "Score"
  | Time -> "Time"
  | Num_in_lobby -> "Num_in_lobby"

(* Broadcasts a message according to our new protocol. Takes in a
   message and command, sends command first then the message. *)

let broadcast_message server message (command : client_command) =
  let command = convert_command_to_string command in
  Ws.Server.broadcast server (command ^ "|" ^ message)

(* Send a message according to our new protocol. Takes in a message and
   command, sends command first then the message. *)
let send_message_to_client client message (command : client_command) =
  let command = convert_command_to_string command in
  Ws.Client.send client (command ^ "|" ^ message)

let on_connect host_id client_states client send_client_message =
  if !check_game_status then begin
    send_client_message
      "Game has already started, please wait for the current game to \
       end."
      Alert
    |> ignore;
    Ws.Server.close server client
  end
  else
    let client_id = get_client_id client in
    Hashtbl.add client_states client_id
      {
        combo_array = [||];
        score = -1;
        combo = "__INIT_COMBO";
        total_game_time = -1;
      };
    if not @@ exists_multiple () then begin
      host_id := client_id;
      send_client_message
        "You're the host! Type /start to start the game." Alert
      |> ignore
    end
    else
      begin
        send_client_message "Waiting for the host to start the game..."
          Alert
        |> ignore
      end;

    broadcast_message server
      (string_of_int @@ Ws.Server.current_connections server)
      Num_in_lobby

let get_client_state
    (client_states : (int, client_info) Hashtbl.t)
    client_id =
  match Hashtbl.find_opt client_states client_id with
  | Some { combo_array; score; combo; total_game_time } ->
      { combo_array; score; combo; total_game_time }
  | None ->
      print_endline "Client not found: get_client_state";
      failwith ""

(* Number of people in the server *)
let get_num_in_server () = server |> Ws.Server.current_connections

(* This function should somehow be run asynchronously, at every tick,
   after the game starts *)
let rec game_loop client_states starting_time () =
  let rec update_clients = function
    | [] -> []
    | c :: tail ->
        let client_id = get_client_id c in
        let send_client_message = send_message_to_client c in
        let total_time =
          match get_client_state client_states client_id with
          | { total_game_time } -> total_game_time
        in
        send_client_message
          (string_of_int
          @@ Game.Timer.time_left starting_time total_time ())
          Time
        |> ignore;
        if Game.Timer.game_over starting_time total_time then (
          (let%lwt _ = send_client_message "Thanks for playing!" Quit in
           Ws.Server.close server c)
          |> ignore;
          update_clients tail)
        else c :: update_clients tail
  in
  let clients = Ws.Server.clients server in
  match clients with
  | [] ->
      print_endline "No clients!";
      check_game_status := false;
      Lwt.return_unit
  | [ c ] ->
      check_game_status := false;
      let%lwt _ = send_message_to_client c "You won!" Alert in
      Lwt.return ()
  | _ ->
      let%lwt _ = Lwt_unix.sleep time_interval in
      let _ = update_clients clients in
      game_loop client_states starting_time ()

let return_tuple client_states client_id =
  match Hashtbl.find_opt client_states client_id with
  | Some { total_game_time; score; combo_array; combo } ->
      (combo_array, score, combo, total_game_time)
  | None ->
      print_endline "Client not found";
      failwith ""

let run_game
    starting_time
    (client_states : (int, client_info) Hashtbl.t)
    client
    message =
  let open Game.Play in
  let open Game.Combinations in
  let open Game.Valid_solution_checker in
  let _ = print_endline @@ "run_game run|" ^ message ^ "|" in
  let client_id = get_client_id client in
  let print_client msg = send_message_to_client client msg Msg in
  let combo_array, score, comb, total_time =
    return_tuple client_states client_id
  in
  let time_left = Game.Timer.time_left starting_time total_time in
  let broadcast_client_message = broadcast_message server in
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
      send_message_to_client client comb Problem |> ignore;
      Lwt.return ()
  | [ "skip" ] ->
      let combo_array, score, comb, total_game_time =
        return_tuple client_states client_id
      in
      send_message_to_client client
        ("Nice Attempt! Here's a solution: "
        ^ (String.split_on_char ' ' comb
          |> List.map int_of_string |> Combinations.solution_to))
        Problem
      |> ignore;
      let new_score = if score - 1 > 0 then score - 1 else 0 in
      let new_line = retrieve_combo "" combo_array in
      send_message_to_client client new_line Problem |> ignore;
      Hashtbl.replace client_states client_id
        {
          combo_array;
          score = new_score;
          combo = new_line;
          total_game_time;
        }
      |> ignore;
      send_message_to_client client (string_of_int new_score) Score
  | [ "score" ] ->
      print_client @@ "Current Score: " ^ string_of_int score |> ignore;
      Lwt.return ()
  | [ "/start" ] ->
      let combo_array, score, comb, total_game_time =
        return_tuple client_states client_id
      in
      let line = retrieve_combo comb combo_array in
      (let _ = print_endline @@ line ^ "This run!" in
       broadcast_client_message line Problem |> ignore;
       broadcast_client_message "0" Score |> ignore;
       (* initialize every client in the current lobby *)
       Hashtbl.filter_map_inplace
         (fun acc c ->
           Some { combo_array; score; combo = line; total_game_time })
         client_states)
      |> ignore;
      Lwt.return ()
  | x -> (
      let combo_array, score, comb, total_game_time =
        return_tuple client_states client_id
      in
      let ans = List.fold_left (fun acc x -> acc ^ " " ^ x) "" x in
      match check_solution ans (combo_to_list comb) with
      | Correct ->
          let new_line = retrieve_combo "" combo_array in
          send_message_to_client client
            (string_of_int (score + 1))
            Score
          |> ignore;
          send_message_to_client client new_line Problem |> ignore;
          Hashtbl.replace client_states client_id
            {
              combo_array;
              score = score + 1;
              combo = new_line;
              total_game_time = total_game_time + 5;
            }
          |> ignore;
          Lwt.return ()
      | Incorrect ->
          send_message_to_client client
            "Incorrect, but nice attempt! Try again!" Problem
          |> ignore;
          Lwt.return ()
      | Invalid ->
          send_message_to_client client
            "Invalid input, but nice attempt! Try again!" Problem
          |> ignore;
          Lwt.return ())

(* In this context [message] refers to the message sent by client *)
let rec handler host_id client_states starting_time client message =
  let _ = print_endline "handler run" in
  (let send_client_message = send_message_to_client client in
   match String.split_on_char ' ' message with
   | [ "on_connect" ] ->
       on_connect host_id client_states client send_client_message
   | [ "debug" ] -> (
       let client_id = get_client_id client in
       match get_client_state client_states client_id with
       | { combo_array; combo; score; total_game_time } ->
           print_endline
             ("starting_time: " ^ string_of_int !starting_time);
           print_endline ("client_id: " ^ string_of_int client_id);
           print_endline
             ("combo_array: "
             ^ Array.fold_left
                 (fun acc x -> acc ^ x ^ ", ")
                 "" combo_array);
           print_endline ("score: " ^ string_of_int score);
           print_endline ("comb: " ^ combo);
           print_endline
             ("total_game_time: " ^ string_of_int total_game_time);
           print_endline "";
           Lwt.return ())
   | _ -> Lwt.return ())
  |> ignore;
  if not !check_game_status then
    run_lobby host_id client_states starting_time client message
  else if message = "on_connect" then Lwt.return_unit
  else run_game !starting_time client_states client message

and run_lobby host_id client_states starting_time client message =
  let _ =
    print_endline (string_of_int @@ Ws.Server.current_connections server)
  in
  let _ = print_endline "run_lobby run" in
  let client_id = get_client_id client in
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
        ignore (broadcast_message server "Game started!" Alert);
        starting_time := int_of_float @@ Unix.time ();
        let in_channel =
          open_in ("assets" ^ Filename.dir_sep ^ "combos.txt")
        in
        let combos = Game.Play.get_combination in_channel in
        let _ = print_endline "got combos" in
        (* initialize every client in the current lobby *)
        Hashtbl.filter_map_inplace
          (fun acc c ->
            Some
              {
                combo_array = combos;
                score = 0;
                combo = "";
                total_game_time = 60;
              })
          client_states;
        game_loop client_states !starting_time () |> ignore;
        run_game !starting_time client_states client message)
      else
        let%lwt _ = send_client_message "You are not the host!" Msg in
        Lwt.return ()
  (* | [ "/quit" ] -> if get_client_id client = !host_id &&
     exists_clients then begin let%lwt _ = send_client_message "Quit
     successfully" Quit in Ws.Server.close server client |> ignore; let
     new_host = next_host client_set !host_id in host_id :=
     get_client_id new_host; let%lwt _ = send_message_to_client new_host
     "You are now the new host!" Msg in Lwt.return () end else if
     get_client_id client = !host_id && Hashtbl.length client_set = 0
     then begin check_game_status := false; let%lwt _ =
     send_client_message "Quit successfully" Quit in Ws.Server.close
     server client end else let%lwt _ = send_client_message "Quit
     successfully" Quit in Ws.Server.close server client *)
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

let handle_closing_connection client_states host_id client content =
  let quitting_client_id = get_client_id client in
  let _ = print_endline (string_of_int quitting_client_id ^ "test") in
  if quitting_client_id = !host_id then (
    (* Redo host *)
    let new_host = get_oldest_client_not_equal quitting_client_id in
    host_id := get_client_id new_host;
    send_message_to_client new_host
      "You are the new host! Type /start to begin." Alert
    |> ignore);
  if not @@ exists_multiple () then check_game_status := false;
  Hashtbl.remove client_states quitting_client_id;
  broadcast_message server
    (string_of_int @@ get_num_in_server ())
    Num_in_lobby

let () =
  (* maps client ids to (combo_array, score, comb, total_game_time) *)
  let client_states : (int, client_info) Hashtbl.t =
    Hashtbl.create 50
  in
  (* the float represents the time at which the lobby started*)
  let start_time = ref 0 in
  let host_id = ref 0 in
  let helper_connect temp = Lwt.return () in
  Lwt_main.run
    (Ws.Server.run server (Some helper_connect)
       ~on_close:(handle_closing_connection client_states host_id)
       (handler host_id client_states start_time))
