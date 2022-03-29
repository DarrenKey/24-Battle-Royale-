(** Protocol for sending messages to the client: Sending something to
    the client involves two messages: (1st message): Command

    Valid Commands are: 'Msg' -> Send a message next line

    'Quit' -> Invokes a command to quit, with a quitting message after
    the line

    [2nd message] : Included message, either from "Msg" or "Quit" *)
type client_command =
  | Msg
  | Quit

type client_message = {
  command : client_command;
  msg : string;
}

exception CommandNotReceived
exception MessageNotReceived
exception CommandNotSent

let parse_command message =
  match message with
  | "Msg" -> Msg
  | "Quit" -> Quit
  | _ -> raise CommandNotReceived

(** Get something from the server according to the protocol outline at
    the top. Returns a record with the command and the associatedm
    message.*)
let get_msg_command (websocket : Hyper.websocket) =
  let%lwt received_command = Hyper.receive websocket in
  let received_command =
    match received_command with
    | Some msg -> msg
    | None -> raise CommandNotReceived
  in
  let%lwt received_message = Hyper.receive websocket in
  let received_message =
    match received_message with
    | Some msg -> msg
    | None -> raise MessageNotReceived
  in
  let client_msg, client_msg_resolver = Lwt.wait () in
  Lwt.wakeup client_msg_resolver
    { command = parse_command received_command; msg = received_message };
  client_msg

let rec websocket_loop websocket =
  let%lwt entered_line = Lwt_io.read_line Lwt_io.stdin in
  let%lwt sent_command = Hyper.send websocket entered_line in
  let%lwt client_msg = get_msg_command websocket in
  match client_msg.command with
  | Quit ->
      print_endline client_msg.msg;
      Hyper.close_websocket websocket
  | Msg ->
      print_endline client_msg.msg;
      websocket_loop websocket

let on_connect_websocket websocket =
  let%lwt send_initial_connect = Hyper.send websocket "on_connect" in
  let%lwt client_msg = get_msg_command websocket in
  match client_msg.command with
  | Quit ->
      print_endline client_msg.msg;
      Hyper.close_websocket websocket
  | Msg ->
      print_endline client_msg.msg;
      websocket_loop websocket

let () =
  Lwt_main.run
    (let%lwt websocket = Hyper.websocket "ws://localhost:3000" in
     on_connect_websocket websocket)
  |> ignore
