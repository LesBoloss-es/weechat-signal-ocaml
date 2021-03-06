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

let render_contact (c: ContactInfo.t) =
  let color =
    match c.color with
    | None -> ""
    | Some c -> Colors.to_weechat c
  in
  let+ name =
    match c.address.number with
    | Some num when num = !Storage.username -> Ok "Me"
    | _ -> ContactInfo.nice_name c
  in
  color ^ name ^ Weechat.color "reset"

let render_address (addr: Address.t) =
  let/ _ =
    Storage.Contacts.get addr
    <&> fst
    >>= render_contact
  in
  (* If the above fails, try something less clever. *)
  match addr.number, addr.uuid with
  | Some n, _ -> Ok (if n = !Storage.username then "Me" else n)
  | None, Some uuid -> Ok uuid
  | None, None -> Error "Failed to render this address"

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

let render_quote (q: Quote.t) =
  let+ author = render_address q.author in
  let grey = Weechat.color "250" in
  Format.sprintf "%s[%s%s] > %s%s\n"
    grey author grey
    q.text
    (Weechat.color "reset")

let render_data_message buffer sender (dm: DataMessage.t) =
  let* quote = match dm.quote with
    | Some quote -> render_quote quote
    | None -> Ok ""
  in
  let+ text =
    match dm.attachments, dm.sticker, dm.reaction, dm.body with
    | (_ :: _), _, _, _ -> Error "Not implemented: attachments"
    | _, Some sticker, _, _ ->
      Format.ksprintf Result.ok
        "sent a sticker (%s / %d)" sticker.packID sticker.stickerID
    | _, _, Some reaction, _ ->
      let old_msg =
        match Storage.Messages.get reaction.targetSentTimestamp with
        | Ok msg -> "\"" ^ msg ^ "\""
        | Error _ -> "a message"
      in
      let+ old_sender = render_address reaction.targetAuthor in
      let grey = Weechat.color "250" in
      let reset = Weechat.color "reset" in
      Format.sprintf "reacted with %s  to %s%s%s from %s"
        reaction.emoji
        grey old_msg reset
        old_sender
    | _, _, _, Some body -> Ok body
    | _ -> Error "Weird message with nothing inside"
  in
  Storage.Messages.add dm.timestamp text;
  Weechat.printf buffer "%s\t%s%s" sender quote text

let handle_data_message (sender: Address.t) (dm: DataMessage.t) =
  let* buffer = select_buffer (Some sender) dm.group dm.groupV2 in
  let* sender = render_address sender in
  render_data_message buffer sender dm

let handle_sync_message (sender: Address.t) (sm: SyncMessage.t) =
  match sm with
  | ReadMessages _ -> Ok () (* Ignore read receipts *)
  | Sent stm ->
    let dm = stm.message in
    let* buffer = select_buffer stm.destination dm.group dm.groupV2 in
    let* sender = render_address sender in
    render_data_message buffer sender dm
  | _ -> Helpers.error "Unimplemented kind of SyncMessage"

let message assoc =
  let* data = Json.assoc_get "data" assoc in
  let* env = MessageEnveloppe.of_yojson data in
  let* sender =
    match env.source with
    | Some addr -> Ok addr
    | None -> Error "Unknown sender"
  in
  (* Ignore receipts and typing messages *)
  if env.typ = "RECEIPT" then Ok ()
  else match env.message with
    | (Receipt _ | Typing _) -> Ok ()
    | Data dm -> handle_data_message sender dm
    | Sync sm -> handle_sync_message sender sm
    | Call _ -> Error "Ignoring CallMessage"
    | Nothing -> Error "Not implemented: MessageEnveloppe of type Nothing"
