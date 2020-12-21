open Weechat_api
open Storage
open Helpers.Syntax
open Api
open Signal

(* version: version information from signald *)

let version buffer assoc =
  let+ version = Json.assoc_get "data" assoc >>= VersionMessage.of_yojson in
  let info = Weechat.prefix "network" in
  Weechat.printf buffer "%s%s version %s (branch: %s, commit: %s)"
    info version.name version.version version.branch version.commit

(* subscribed: info from signald *)

let subscribed buffer =
  let info = Weechat.prefix "network" in
  Weechat.printf buffer "%ssubscribed" info;
  Ok ()

(* listen_started: info from signald *)

let listen_started buffer =
  let info = Weechat.prefix "network" in
  Weechat.printf buffer "%slisten started" info;
  Ok ()

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

let render_address (addr: Address.t) =
  match Storage.Contacts.get addr with
  | Ok (c, _) -> ContactInfo.nice_name c
  | Error _ ->
    match addr.number, addr.uuid with
    | Some n, _ -> Ok n
    | None, Some uuid -> Ok uuid
    | None, None -> Error "Cannot render address"

(** Choose a buffer based on a contact / groupV1 / groupV2 *)
let select_buffer (addr: Address.t option)
                  (g1: GroupInfo.t option)
                  (g2: GroupV2Info.t option) =
  let g1 = Option.map (fun gi -> Group.V1 gi) g1 in
  let g2 = Option.map (fun gi -> Group.V2 gi) g2 in
  match addr, g1, g2 with
    | (_, Some g, None | _, None, Some g) -> snd <$> Storage.Groups.get g
    | Some addr, _, _ -> snd <$> Storage.Contacts.get addr
    | _ -> Error "Could not find a buffer for this message"

let render_data_message buffer sender (dm: DataMessage.t) =
  let+ text =
    match dm.attachments, dm.sticker, dm.reaction, dm.body with
    | (_ :: _), _, _, _ -> Error "Not implemented: attachments"
    | _, Some _, _, _ -> Error "Not implemented: stickers"
    | _, _, Some reaction, _ ->
      let old_msg =
        match Storage.Messages.get reaction.targetSentTimestamp with
        | Ok msg -> "\"" ^ msg ^ "\""
        | Error _ -> "a message"
      in
      let+ old_sender = render_address reaction.targetAuthor in
      Format.sprintf "reacted %s to %s from %s"
        reaction.emoji old_msg old_sender
    | _, _, _, Some body -> Ok body
    | _ -> Error "Weird message with nothing inside"
  in
  Storage.Messages.add dm.timestamp text;
  Weechat.printf buffer "%s\t%s" sender text

let handle_data_message (sender: ContactInfo.t) (dm: DataMessage.t) =
  let* buffer = select_buffer (Some sender.address) dm.group dm.groupV2 in
  let* name = ContactInfo.nice_name sender in
  render_data_message buffer name dm

let handle_sync_message (sender: ContactInfo.t) (sm: SyncMessage.t) =
  match sm with
  | ReadMessages _ -> Ok () (* Ignore read receipts *)
  | Sent stm ->
    let dm = stm.message in
    let* buffer = select_buffer stm.destination dm.group dm.groupV2 in
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
  (* Ignore receipts and typing messages *)
  | (Receipt _ | Typing _) -> Ok()
