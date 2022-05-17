open Lwt_io
open Lwt

(* Protocol for sending messages to the client: Sending something to the
   client involves two messages: (1st message): Command

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

(* Get something from the server according to the protocol outline at
   the top. Returns a record with the command and the associated
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
      printl client_msg.msg |> ignore;
      Hyper.close_websocket websocket
  | Msg ->
      printl client_msg.msg |> ignore;
      websocket_loop websocket

let rec send_websocket_loop websocket =
  let%lwt entered_line = Lwt_io.read_line Lwt_io.stdin in
  let%lwt sent_command = Hyper.send websocket entered_line in
  send_websocket_loop websocket

let on_connect_websocket websocket =
  let%lwt send_initial_connect = Hyper.send websocket "on_connect" in
  send_websocket_loop websocket

let rec msg_listener websocket =
  let%lwt client_msg = get_msg_command websocket in

  match client_msg.command with
  | Quit ->
      printl client_msg.msg |> ignore;
      msg_listener websocket
  | Msg ->
      printl client_msg.msg |> ignore;
      msg_listener websocket

let rec check_message_interval interval websocket =
  let full_interval = Lwt_unix.sleep interval in
  let half_interval = Lwt_unix.sleep (interval /. 2.) in
  let msg =
    Lwt.bind (get_msg_command websocket) (fun actual_msg ->
        match actual_msg.command with
        | Msg -> printl actual_msg.msg
        | Quit ->
            printl actual_msg.msg |> ignore;
            Hyper.close_websocket websocket |> ignore;
            exit 0)
  in
  Lwt.bind half_interval (fun _ ->
      Lwt.cancel msg;
      Lwt.return ())
  |> ignore;
  Lwt.bind full_interval (fun _ ->
      check_message_interval interval websocket)

let () =
  let websocket = Hyper.websocket "ws://localhost:3000" in
  Lwt_main.run
    (let%lwt websocket = websocket in
     let _ = check_message_interval 0.1 websocket in
     on_connect_websocket websocket)
  |> ignore;
  ()
