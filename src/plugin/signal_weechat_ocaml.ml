open Weechat_api
open Api
open Signal
open Helpers
open Syntax

let signal_command_cb socket buffer argv argv_eol =
  let argc = Array.length argv in
  if argc < 2 then Error "/signal: argument required"
  else match argv.(1) with
    | "subscribe" ->
      if argc <> 3 then Error "/signal subscribe: wrong number of arguments"
      else begin
        Storage.username := argv.(2);
        let* () = Socket.subscribe socket !Storage.username in
        let* () = Socket.list_groups socket !Storage.username in
        Socket.list_contacts socket !Storage.username
      end
    | "sync" ->
      if argc <> 2 then Error "/signal sync: wrong number of arguments"
      else if !Storage.username = "" then Error "Please subscribe first"
      else
        let* () = Socket.list_groups socket !Storage.username in
        let* () = Socket.list_contacts socket !Storage.username in
        Ok ()
    | _ -> Error "/signal: invalid argument"

let process_line buffer line =
  let json = Json.from_string line in
  let* assoc = Json.as_assoc json in
  let* typ = Json.assoc_get "type" assoc >>= Json.as_string in
  match typ with
  | "version" -> Handlers.version buffer assoc
  | "subscribed" -> Handlers.subscribed buffer
  | "listen_started" -> Handlers.listen_started buffer
  | "group_list" -> Handlers.group_list assoc
  | "contact_list" -> Handlers.contact_list assoc
  | "message" -> Handlers.message assoc
  | _ -> error "unhandled input"


let plugin_init () =
  let main_buffer =
    Weechat.buffer_new
      "signald-ocaml"
      (fun buffer _ -> Weechat.printf buffer "[read-only buffer]"; 0)
      (fun _ -> 0)
  in

  let error msg =
    let prefix = Weechat.prefix "error" in
    Weechat.printf main_buffer "%s%s" prefix msg;
    -1
  in

  let socket = Socket.create "/run/signald/signald.sock" in

  let _ = Weechat.hook_command
    "signal"
    "Interact with signal-weechat-ocaml"
    "sync | subscribe NUMBER"
    "sync: refresh contact and group list\n\
     subscribe: start receiving message for the given account"
     "sync || subscribe"
     (fun buf argv argv_eol ->
       match signal_command_cb socket buf argv argv_eol with
       | Ok () -> 0
       | Error msg -> error msg)
  in

  let _ = Weechat.hook_fd
    (Obj.magic socket.socket)
    ~flag_read:true
    ~flag_write:false
    (fun _ ->
      try
        let line = input_line socket.ic in
        begin match process_line main_buffer line with
          | Ok () -> 0
          | Error msg ->
            let _ = error ("On input: " ^ line) in
            error msg
        end
      with End_of_file -> 0)
  in

  socket
  

let () =
  let socket = ref None in
  Weechat_plugin.define
    ~plugin_init:(fun () -> socket := Some (plugin_init ()))
    ~plugin_end:(fun () ->
      match !socket with
      | Some sock -> Socket.close sock;
      | None -> ();
      Weechat.plugin_end ())
