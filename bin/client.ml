(** Protocol for sending messages to the client: Sending something to
    the client involves two messages: (1st message): Command

    Valid Commands are: 'Msg' -> Send a message next line

    'Quit' -> Invokes a command to quit, with a quitting message after
    the line

    [2nd message] : Included message, either from "Msg" or "Quit" *)

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
  let%lwt send_initial_connect = Hyper.send websocket "on_connect" in
  let%lwt sent_line = Hyper.receive websocket in
  (match sent_line with
  | Some x ->
      print_endline x;
      websocket_loop websocket
  | None ->
      print_endline "nothing sent";
      Lwt.return ())
  |> ignore;
  websocket_loop websocket

let () =
  Lwt_main.run
    (let%lwt websocket = Hyper.websocket "ws://localhost:3000" in
     on_connect_websocket websocket)
  |> ignore
