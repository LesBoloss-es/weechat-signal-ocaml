type t =
  | Contacts of Attachment.t
  | Sent of SentTranscriptMessage.t
  | Groups of Attachment.t
  | BlockedList of BlockedListMessage.t
  | Configuration of ConfigurationMessage.t
  | FetchType of string
  | MessageRequestResponse of MessageRequestResponseMessage.t
  | ReadMessages of ReadMessage.t list
  | Request of string
  | StickerPackOperations of StickerPackOperationMessage.t list
  | Verified of VerifiedMessage.t
  | ViewOnceOpen of ViewOnceOpenMessage.t


let of_yojson =
  let open Helpers.Syntax in

  let parse_other name data =
    match name with
    | "contacts" ->
      let+ x = Attachment.of_yojson data in
      Contacts x
    | "sent" ->
      let+ x = SentTranscriptMessage.of_yojson data in
      Sent x
    | "groups" ->
      let+ x = Attachment.of_yojson data in
      Groups x
    | "blockedList" ->
      let+ x = BlockedListMessage.of_yojson data in
      BlockedList x
    | "configuration" ->
      let+ x = ConfigurationMessage.of_yojson data in
      Configuration x
    | "fetchType" ->
      let+ x = [%of_yojson: string] data in
      FetchType x
    | "messageRequestResponse" ->
      let+ x = MessageRequestResponseMessage.of_yojson data in
      MessageRequestResponse x
    | "readMessages" ->
      let+ x = [%of_yojson: ReadMessage.t list] data in
      ReadMessages x
    | "request" ->
      let+ x = [%of_yojson: string] data in
      Request x
    | "stickerPackOperations" ->
      let+ x = [%of_yojson: StickerPackOperationMessage.t list] data in
      StickerPackOperations x
    | "verified" ->
      let+ x = VerifiedMessage.of_yojson data in
      Verified x
    | "viewOnceOpen" ->
      let+ x = ViewOnceOpenMessage.of_yojson data in
      ViewOnceOpen x
    | _ -> Error "SyncMessage.t"
  in

  fun json -> match json with
    | `Assoc [("contactsComplete", b); (name, data)]
    | `Assoc [(name, data); ("contactsComplete", b)] ->
      let* msg = parse_other name data in
      let* b = [%of_yojson: bool] b in
      if (name = "contacts") = b then Ok msg
      else Error "Something is wrong with contactsComplete"

    | `Assoc l ->
      Helpers.error "I received a syncMessage with %d field(s)" (List.length l)
    | _ -> Error "SyncMessage.t"
