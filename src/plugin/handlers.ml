open Weechat_api
open Storage
open Helpers.Syntax
open Api
open Signal

(* subscribed: info from signald *)

let subscribed buffer =
  match Weechat.prefix "network" with
  | Some info -> Ok (Weechat.printf buffer "%ssubscribed" info)
  | None -> Error "Weechat.prefix error"

(* listen_started: info from signald *)

let listen_started buffer =
  match Weechat.prefix "network" with
  | Some info -> Ok (Weechat.printf buffer "%slisten started" info)
  | None -> Error "Weechat.prefix error"

(* group_list: update the list of known groups *)

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

(* contact_list: update the list of known contacts *)

let contact_list assoc =
  Json.assoc_get "data" assoc
  >>= Json.as_list
  >>= Helpers.try_list_map (ContactInfo.of_yojson)
  >>= Helpers.try_list_iter Contacts.update

(* message: handle the various kinds of messages *)

(** Choose a buffer based on a contact / groupV1 / groupV2 *)
let select_buffer (addr: Address.t) (g1: GroupInfo.t option)
                                    (g2: GroupV2Info.t option) =

  let g1 = Option.map (fun gi -> Group.V1 gi) g1 in
  let g2 = Option.map (fun gi -> Group.V2 gi) g2 in
  match g1, g2 with
    | (Some g, None | None, Some g) -> snd <$> Storage.Groups.get g
    | _ -> snd <$> Storage.Contacts.get addr

let render_data_message buffer sender (dm: DataMessage.t) =
  match dm.attachments, dm.sticker, dm.reaction, dm.body with
  | (_ :: _), _, _, _ -> Error "Not implemented: attachments"
  | _, Some _, _, _ -> Error "Not implemented: stickers"
  | _, _, Some _, _ -> Error "Not implemented: reactions"
  | _, _, _, Some body -> Ok (Weechat.printf buffer "%s\t%s" sender body)
  | _ -> Error "Weird message with nothing inside"

let handle_data_message (sender: ContactInfo.t) (dm: DataMessage.t) =
  let* buffer = select_buffer sender.address dm.group dm.groupV2 in
  let* name = ContactInfo.nice_name sender in
  render_data_message buffer name dm

let handle_sync_message (sender: ContactInfo.t) (sm: SyncMessage.t) =
  match sm with
  | ReadMessages _ -> Ok () (* Ignore read receipts *)
  | Sent stm ->
    let* dest = match stm.destination with
      | Some addr -> Ok addr
      | None -> Error "Sent message with no destination"
    in
    let dm = stm.message in
    let* buffer = select_buffer dest dm.group dm.groupV2 in
    let* number = Address.number sender.address in
    let* name =
      if number = !Storage.username
      then Ok "Me"
      else ContactInfo.nice_name sender
    in
    render_data_message buffer name dm
  | _ -> Helpers.error "Unimplemented kind of SyncMessage"

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
