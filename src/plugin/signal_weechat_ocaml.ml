open Weechat_api
open Signal
open Syntax

module Groups = struct
  let table = Hashtbl.create 17

  let add_if_absent (g: Group.t) =
    if not (Hashtbl.mem table g.id) then
      let buffer =
        Weechat.buffer_new
          g.title
          (fun buffer _ ->
            Weechat.printf buffer "[writing to group not yet implemented]";
            0)
          (fun _ -> 0)
      in
      Hashtbl.add table g.id (g, buffer)
end

module Config = struct
  let username = ref ""
end

let signal_command_cb socket buffer argv argv_eol =
  let argc = Array.length argv in
  if argc < 2 then Error "/signal: argument required"
  else match argv.(1) with
    | "username" ->
      if argc != 3 then Error "/signal username: wrong number of arguments"
      else Ok (Config.username := argv.(2))
    | "subscribe" ->
      if argc != 2 then Error "/signal subscribe: wrong number of arguments"
      else Socket.subscribe socket !Config.username
    | "list" ->
      if argc != 3 then Error "/signal list: wrong number of arguments"
      else begin match argv.(2) with
        | "groups" ->
          if !Config.username = ""
          then Error "Please set your username first"
          else Socket.list_groups socket !Config.username
        | _ -> Error "/signal list: wrong argument"
      end
    | _ -> Error "/signal: invalid argument"

let process_line buffer line =
  let json = Json.from_string line in
  let* assoc = Json.as_assoc json in
  let* typ = Json.assoc_get "type" assoc >>= Json.as_string in
  match typ with
  | "group_list" ->
    let* data = Json.assoc_get "data" assoc in
    let+ groups = Group.parse_group_list data in
    List.iter (fun g -> Groups.add_if_absent g) groups
  | _ ->
    Ok (Weechat.printf buffer "unhandled message from signald: %s" line)

let plugin_init () =
  let main_buffer =
    Weechat.buffer_new
      "signald-ocaml"
      (fun buffer _ -> Weechat.printf buffer "[read-only buffer]"; 0)
      (fun _ -> 0)
  in

  let error msg =
    Weechat.printf main_buffer "[Error] %s" msg;
    -1
  in

  let socket = Socket.create "/run/signald/signald.sock" in

  let _ = Weechat.hook_command
    "signal"
    "Interact with signal-weechat-ocaml"
    "list groups | subscribe | username NUMBER "
    "list groups: list known groups\n\
     subscribe: start receiving message\n\
     username: set the number/username of the current account"
     "list groups || subscribe || username"
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
        begin match process_line main_buffer (input_line socket.ic) with
          | Ok () -> 0
          | Error msg -> error msg
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
