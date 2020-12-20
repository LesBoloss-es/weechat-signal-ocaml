open Weechat_api
open Storage
open Helpers.Syntax
open Api
module Json = Signal.Json

let subscribed buffer =
  match Weechat.prefix "network" with
  | Some info -> Ok (Weechat.printf buffer "%ssubscribed" info)
  | None -> Error "Weechat.prefix error"

let listen_started buffer =
  match Weechat.prefix "network" with
  | Some info -> Ok (Weechat.printf buffer "%slisten started" info)
  | None -> Error "Weechat.prefix error"

let group_list assoc =
  let* group_list = Json.assoc_get "data" assoc >>= GroupList.of_yojson in
  let* () =
    Helpers.try_list_iter
      (fun gi -> Groups.update (V1 gi))
      group_list.groups
  in
  Helpers.try_list_iter
    (fun gi -> Groups.update (V2 gi))
    group_list.groupsv2

let contact_list assoc =
  Json.assoc_get "data" assoc
  >>= Json.as_list
  >>= Helpers.try_list_map (ContactInfo.of_yojson)
  >>= Helpers.try_list_iter Contacts.update
