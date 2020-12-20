type low_level = {
  username: string;

  uuid: string option [@default None];
  source: Address.t option [@default None];
  sourceDevice: int;
  relay: string option [@default None];

  typ: string [@key "type"];
  timestamp: int;
  timestampISO: string;
  serverTimestamp: int;
  serverDeliveredTimestamp: int;
  hasLegacyMessage: bool;
  hasContent: bool;
  isUnidentifiedSender: bool;

  callMessage: CallMessage.t option [@default None];
  dataMessage: DataMessage.t option [@default None];
  receipt: ReceiptMessage.t option [@default None];
  syncMessage: SyncMessage.t option [@default None];
  typing: TypingMessage.t option [@default None];
} [@@deriving of_yojson]

type message =
  | Call of CallMessage.t
  | Data of DataMessage.t
  | Receipt of ReceiptMessage.t
  | Sync of SyncMessage.t
  | Typing of TypingMessage.t

type t = {
  username: string;

  uuid: string option;
  source: Address.t option;
  sourceDevice: int;
  relay: string option;

  typ: string;
  timestamp: int;
  timestampISO: string;
  serverTimestamp: int;
  serverDeliveredTimestamp: int;
  hasLegacyMessage: bool;
  hasContent: bool;
  isUnidentifiedSender: bool;

  message: message;
}

let of_yojson json =
  let open Helpers.Syntax in
  let* ll = low_level_of_yojson json in
  let+ message =
    match ll.callMessage, ll.dataMessage, ll.receipt, ll.syncMessage, ll.typing
    with
    | Some cm, None,    None,   None,    None   -> Ok (Call cm)
    | None,    Some dm, None,   None,    None   -> Ok (Data dm)
    | None,    None,    Some r, None,    None   -> Ok (Receipt r)
    | None,    None,    None,   Some sm, None   -> Ok (Sync sm)
    | None,    None,    None,   None,    Some t -> Ok (Typing t)
    | _ -> Error "invalid message"
  in
  {
    username = ll.username;
    uuid = ll.uuid;
    source = ll.source;
    sourceDevice = ll.sourceDevice;
    relay = ll.relay;

    typ = ll.typ;
    timestamp = ll.timestamp;
    timestampISO = ll.timestampISO;
    serverTimestamp = ll.serverTimestamp;
    serverDeliveredTimestamp = ll.serverDeliveredTimestamp;
    hasLegacyMessage = ll.hasLegacyMessage;
    hasContent = ll.hasContent;
    isUnidentifiedSender = ll.isUnidentifiedSender;

    message = message
}
