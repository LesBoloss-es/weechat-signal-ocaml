open Weechat_api
open Signal
open Syntax

module Contacts = struct
  let table = Hashtbl.create 17

  let update (c: Contact.t) =
    match Hashtbl.find_opt table c.uuid with
    | None ->
      let buffer = Weechat.buffer_new
        (Contact.nice_name c)
        (fun buffer _ ->
          Weechat.printf buffer "[writing to contact not yet implemented]";
          0)
        (fun _ -> 0)
      in
      Hashtbl.add table c.uuid (c, buffer)
    | Some (c', buffer) ->
      if c'.name <> c.name then begin
        Hashtbl.replace table c.uuid (c, buffer)
        (* TODO: rename the buffer *)
      end
end

module Groups = struct
  let table = Hashtbl.create 17

  let update (g: Group.t) =
    match Hashtbl.find_opt table g.id with
    | None ->
      let buffer =
        Weechat.buffer_new
          (if g.title <> "" then g.title else g.id)
          (fun buffer _ ->
            Weechat.printf buffer "[writing to group not yet implemented]";
            0)
          (fun _ -> 0)
      in
      Hashtbl.add table g.id (g, buffer)
    | Some (g', buffer) ->
      if g'.title <> g.title then begin
        Hashtbl.replace table g.id (g, buffer);
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
      if argc <> 3 then Error "/signal sync: wrong number of arguments"
      else begin match argv.(2) with
        | "groups" ->
          if !Config.username = ""
          then Error "Please subscribe first"
          else Socket.list_groups socket !Config.username
        | _ -> Error "/signal sync: invalid argument"
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
    List.iter Groups.update groups
  | "contact_list" ->
    let* data = Json.assoc_get "data" assoc in
    let+ contacts = Contact.parse_contact_list data in
    List.iter Contacts.update contacts
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
    "sync [groups | contacts] | subscribe NUMBER"
    "sync [groups | contacts]: refresh the list of contacts/groups\n\
     subscribe: start receiving message for the given account"
     "sync groups || sync contacts || subscribe"
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
