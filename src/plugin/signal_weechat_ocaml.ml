open Weechat_api

open Api
open Signal
open Helpers
open Syntax

module Contacts = struct
  let table = Hashtbl.create 17

  let update (c: ContactInfo.t) =
    let* number = Address.number c.address in
    let+ name = ContactInfo.name c <|> Ok number in
    match Hashtbl.find_opt table number with
    | None ->
      let buffer = Weechat.buffer_new
        name
        (fun buffer _ ->
          Weechat.printf buffer "[writing to contact not yet implemented]";
          0)
        (fun _ -> 0)
      in
      Hashtbl.add table number (c, buffer)
    | Some (c', buffer) ->
      if c' <> c then begin
        Hashtbl.replace table number (c, buffer)
        (* TODO: rename the buffer *)
      end
end

module Groups = struct
  type item = Group.t * Weechat.gui_buffer
  let table: (string, item) Hashtbl.t = Hashtbl.create 17

  let update group =
    let* id = Group.id group in
    let+ title = Group.title group <|> Ok id in
    match Hashtbl.find_opt table id with
    | None ->
      let buffer =
        Weechat.buffer_new
          title
          (fun buffer _ ->
            Weechat.printf buffer "[writing to group not yet implemented]";
            0)
          (fun _ -> 0)
      in
      Hashtbl.add table id (group, buffer)
    | Some (group', buffer) ->
      if group' <> group then begin
        Hashtbl.replace table id (group, buffer);
        (* TODO: rename the buffer *)
      end
end

module Config = struct
  let username = ref ""
end

let signal_command_cb socket buffer argv argv_eol =
  let argc = Array.length argv in
  if argc < 2 then Error "/signal: argument required"
  else match argv.(1) with
    | "subscribe" ->
      if argc <> 3 then Error "/signal subscribe: wrong number of arguments"
      else begin
        Config.username := argv.(2);
        let* () = Socket.subscribe socket !Config.username in
        let* () = Socket.list_groups socket !Config.username in
        Socket.list_contacts socket !Config.username
      end
    | "sync" ->
      if argc <> 2 then Error "/signal sync: wrong number of arguments"
      else if !Config.username = "" then Error "Please subscribe first"
      else
        let* () = Socket.list_groups socket !Config.username in
        let* () = Socket.list_contacts socket !Config.username in
        Ok ()
    | _ -> Error "/signal: invalid argument"

let process_line buffer line =
  let json = Json.from_string line in
  let* assoc = Json.as_assoc json in
  let* typ = Json.assoc_get "type" assoc >>= Json.as_string in
  match typ with
  | "group_list" ->
    let* group_list = Json.assoc_get "data" assoc >>= GroupList.of_yojson in
    let* () = try_list_iter (fun gi -> Groups.update (V1 gi)) group_list.groups in
    try_list_iter (fun gi -> Groups.update (V2 gi)) group_list.groupsv2
  | "contact_list" ->
    let* contact_list =
      Json.assoc_get "data" assoc
      >>= Json.as_list
      >>= try_list_map (ContactInfo.of_yojson)
    in
    try_list_iter Contacts.update contact_list
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
