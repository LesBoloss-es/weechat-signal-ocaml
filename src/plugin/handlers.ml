open Weechat_api
open Storage
open Helpers.Syntax
open Api
open Signal

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

let handle_data_message (sender: ContactInfo.t) (dm: DataMessage.t) =
  let group =
    match dm.group, dm.groupV2 with
    | Some gi, None -> Some (Group.V1 gi)
    | None, Some gi -> Some (Group.V2 gi)
    | _ -> None
  in
  let* buffer =
    match group with
    | Some g -> snd <$> Storage.Groups.get g
    | None -> snd <$> Storage.Contacts.get sender.address
  in
  let* name = ContactInfo.nice_name sender in
  match dm.attachments, dm.sticker, dm.reaction, dm.body with
  | (_ :: _), _, _, _ -> Error "Not implemented: attachments"
  | _, Some _, _, _ -> Error "Not implemented: stickers"
  | _, _, Some _, _ -> Error "Not implemented: reactions"
  | _, _, _, Some body -> Ok (Weechat.printf buffer "%s\t%s" name body)
  | _ -> Error "Weird message with nothing inside"

let handle_sync_message sender sm =
  ignore sm;
  Error "Not implemented: SyncMessage"

let message assoc =
  let* data = Json.assoc_get "data" assoc in
  let* env = MessageEnveloppe.of_yojson data in
  let* sender, _ =
    match env.source with
    | Some a -> Storage.Contacts.get a
    | None -> Error "Unknown sender"
  in
  match env.message with
  | Data dm -> handle_data_message sender dm
  | Sync sm -> handle_sync_message sender sm
  | Call _ -> Error "Ignoring CallMessage"
  | Receipt _ -> Error "Ignoring ReceiptMessage"
  | Typing _ -> Error "Ignoring TypingMessage"
