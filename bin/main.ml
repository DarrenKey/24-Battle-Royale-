let string_of_client client =
  client |> Ws.Client.id |> Ws.Client.Id.to_int |> string_of_int

let server = Ws.Server.create ~port:3000
let get_client_id client = client |> Ws.Client.id |> Ws.Client.Id.to_int

let on_connect client_set host_id client =
  if Hashtbl.length client_set = 0 then
    (host_id := get_client_id client;
     Ws.Client.send client
       "You're the host! Type /start to start the game.")
    |> ignore
  else
    Ws.Client.send client "Waiting for the host to start the game..."
    |> ignore;
  Hashtbl.add client_set (get_client_id client) false;
  Lwt.return ()

let run_start_setup server client =
  Ws.Server.broadcast_to_others server client "Game starting!"

let handler client_set host_id client message =
  let client_id = get_client_id client in
  match String.split_on_char ' ' message with
  | [ "debug_host" ] -> Ws.Client.send client (!host_id |> string_of_int)
  | [ "/start" ] ->
      if !host_id = client_id then run_start_setup server client
      else Ws.Client.send client "You are not the host!"
  | [ "/quit" ] ->
      Hashtbl.remove client_set client_id;
      Ws.Server.close server client
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
  let client_set : (int, bool) Hashtbl.t = Hashtbl.create 50 in
  let host_id = ref 0 in
  let helper_connect = on_connect client_set host_id in
  Lwt_main.run
    (Ws.Server.run server (Some helper_connect)
       (handler client_set host_id))
