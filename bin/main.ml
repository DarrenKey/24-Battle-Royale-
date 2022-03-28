let string_of_client client =
  client |> Ws.Client.id |> Ws.Client.Id.to_int |> string_of_int

let server = Ws.Server.create ~port:3000
let get_client_id client = client |> Ws.Client.id |> Ws.Client.Id.to_int
let check_game_status = ref false

let on_connect client_set host_id client =
  if !check_game_status then begin
    Ws.Client.send client
      "Game has already started, please wait for the current game to \
       end."
    |> ignore;
    Ws.Server.close server client |> ignore
  end
  else begin
    if Hashtbl.length client_set = 0 then
      (host_id := get_client_id client;
       Ws.Client.send client
         "You're the host! Type /start to start the game.")
      |> ignore
    else
      Ws.Client.send client "Waiting for the host to start the game..."
      |> ignore;
    Hashtbl.add client_set (get_client_id client) client
  end;
  Lwt.return ()

let run_start_setup server client =
  check_game_status := true;
  Ws.Server.broadcast server "Game starting!"

let rec next_host client_set host_id : Ws.Client.t =
  let pos_host_id = host_id + 1 in
  match Hashtbl.find_opt client_set pos_host_id with
  | Some client -> client
  | None -> next_host client_set pos_host_id

let handler client_set host_id client message =
  let client_id = get_client_id client in
  match String.split_on_char ' ' message with
  | [ "debug_host" ] -> Ws.Client.send client (!host_id |> string_of_int)
  | [ "/start" ] ->
      if !host_id = client_id then run_start_setup server client
      else Ws.Client.send client "You are not the host!"
  | [ "/quit" ] ->
      Hashtbl.remove client_set client_id;
      if
        get_client_id client = !host_id
        && Hashtbl.length client_set >= 1
      then begin
        Ws.Server.close server client |> ignore;
        let new_host = next_host client_set !host_id in
        host_id := get_client_id new_host;
        Ws.Client.send new_host "You are now the new host!"
      end
      else if
        get_client_id client = !host_id && Hashtbl.length client_set = 0
      then begin
        check_game_status := false;
        Ws.Server.close server client
      end
      else Ws.Server.close server client
  | [ a; b; c; d; e ] when a = "evaluate" ->
      Game.Combinations.Combinations.solution_to
        (List.map int_of_string [ b; c; d; e ])
      |> Ws.Client.send client
  | broadcast_message ->
      Ws.Server.broadcast_to_others server client
        (Printf.sprintf "%s: %s"
           (string_of_client client)
           (String.concat " " broadcast_message))

let () =
  let client_set : (int, Ws.Client.t) Hashtbl.t = Hashtbl.create 50 in
  let host_id = ref 0 in
  let helper_connect = on_connect client_set host_id in
  Lwt_main.run
    (Ws.Server.run server (Some helper_connect)
       (handler client_set host_id))
