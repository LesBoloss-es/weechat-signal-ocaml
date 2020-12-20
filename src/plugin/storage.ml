open Api
open Signal
open Helpers.Syntax
open Weechat_api

module Contacts = struct
  let table = Hashtbl.create 17

  let update (c: ContactInfo.t) =
    let* number = Address.number c.address in
    let+ name = ContactInfo.nice_name c in
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

  let get (g: Group.t) =
    let* id = Group.id g in
    match Hashtbl.find_opt table id with
    | None -> Error "Group not found"
    | Some (g, b) -> Ok (g, b)
end

let username = ref ""
