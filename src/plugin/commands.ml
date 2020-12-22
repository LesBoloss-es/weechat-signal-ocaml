open Weechat_api
open Helpers.Syntax
open Signal
open Api

let subscribe socket argv =
  if Array.length argv <> 3 then
    Error "subscribe: wrong number of arguments, expected one"
  else begin
    let username = argv.(2) in
    Storage.username := username;
    let* () = Socket.subscribe socket username in
    let* () = Socket.list_groups socket username in
    let* () = Socket.list_contacts socket username in
    Ok ()
  end


let sync socket argv =
  if Array.length argv <> 2 then
    Error "sync: unexpected arguments"
  else begin
    let* () = Socket.list_groups socket !Storage.username in
    let* () = Socket.list_contacts socket !Storage.username in
    Ok ()
  end


let list buffer argv =
  if Array.length argv <> 2 then
    Error "list: does not handle arguments at the moment"
  else begin
    let info = Weechat.prefix "network" in
    Weechat.printf buffer "%sContacts:" info;
    let* () =
      Helpers.try_seq_iter
        (fun (_, (c, _)) ->
          let* number = Address.number c.ContactInfo.address in
          let+ name =
            if number = !Storage.username then Ok "Not to self"
            else ContactInfo.nice_name c
          in
          Weechat.printf buffer "%s  %s (%s)" info name number)
        (Storage.Contacts.to_seq ())
    in
    Weechat.printf buffer "%sGroups:" info;
    let* () =
      Helpers.try_seq_iter
        (fun (_, (g, _)) ->
          let+ title = Group.title g in
          Weechat.printf buffer "%s  %s" info title)
        (Storage.Groups.to_seq ())
    in
    Ok ()
  end
