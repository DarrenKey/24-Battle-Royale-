let rec websocket_loop websocket =
  let%lwt entered_line = Lwt_io.read_line Lwt_io.stdin in
  let%lwt sent_command = Hyper.send websocket entered_line in
  let%lwt received = Hyper.receive websocket in
  match received with
  | Some x ->
      print_endline x;
      websocket_loop websocket
  | None ->
      Lwt.return ();
      websocket_loop websocket

let on_connect_websocket websocket =
  let%lwt sent_line = Hyper.receive websocket in
  (match sent_line with
  | Some x ->
      print_endline x;
      websocket_loop websocket
  | None -> Lwt.return ());
  websocket_loop websocket

let () =
  Lwt_main.run
    (let%lwt websocket = Hyper.websocket "ws://localhost:3000" in
     websocket_loop websocket)
  |> ignore
