open Signal
open Helpers.Syntax

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
