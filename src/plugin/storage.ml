open Api
open Signal
open Helpers.Syntax
open Weechat_api

let username = ref ""
let socket = ref None

module Messages = struct
  let table: (Int64.t, string) Hashtbl.t = Hashtbl.create 1063

  let add ts text =
    let text =
      if String.length text < 200 then text
      else begin
        let s = Bytes.create 200 in
        Bytes.blit_string text 0 s 0 197;
        Bytes.blit_string "..." 0 s 197 3;
        Bytes.unsafe_to_string s
      end
    in
    Hashtbl.replace table ts text

  let get ts =
    match Hashtbl.find_opt table ts with
    | None -> Error "message not found"
    | Some text -> Ok text
end

let send_callback destination buffer text =
  if !username = ""
  then begin
    Weechat.printf buffer "[please subscribe first]";
    -1
  end else match !socket with
    | None ->
      Weechat.printf buffer "[socket not found]";
      -1
    | Some sock ->
      let username = !username in
      match Action.send ~destination ~messageBody:(Some text) ~username sock with
      | Ok () ->
        Weechat.printf buffer "Me\t%s" text;
        0
      | Error err ->
        Weechat.printf buffer "%s%s" (Weechat.prefix "error") err;
        -1

module Contacts = struct
  let table = Hashtbl.create 17

  let update (c: ContactInfo.t) =
    let* number = Address.number c.address in
    let+ name =
      if number = !username then Ok "Note to self"
      else ContactInfo.nice_name c
    in
    match Hashtbl.find_opt table number with
    | None ->
      let buffer = Weechat.buffer_new
        name
        (send_callback (Address c.address))
        (fun _ -> 0)
      in
      Hashtbl.add table number (c, buffer)
    | Some (c', buffer) ->
      if c' <> c then begin
        Hashtbl.replace table number (c, buffer)
        (* TODO: rename the buffer *)
      end

  let get (a: Address.t) =
    let* number = Address.number a in
    match Hashtbl.find_opt table number with
    | None -> Error "Contact not found"
    | Some (c, b) -> Ok (c, b)
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
          (send_callback (Group group))
          (fun _ -> 0)
      in
      Hashtbl.add table id (group, buffer)
    | Some (group', buffer) ->
      if group' <> group then begin
        Hashtbl.replace table id (group, buffer);
        (* TODO: rename the buffer *)
      end

  let get (g: Group.t) =
    let* id = Group.id g in
    match Hashtbl.find_opt table id with
    | None -> Error "Group not found"
    | Some (g, b) -> Ok (g, b)
end
