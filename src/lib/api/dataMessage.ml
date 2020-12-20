type t = {
  timestamp:        Int64.t;
  attachments:      Attachment.t list     [@default []];
  body:             string option         [@default None];
  group:            GroupInfo.t option    [@default None];
  groupV2:          GroupV2Info.t option  [@default None];
  endSession:       bool;
  expiresInSeconds: int; (* Zero means: never expires. *)
  profileKeyUpdate: bool;
  quote:            Quote.t option        [@default None];
  contacts:         SharedContact.t list  [@default []];
  previews:         Preview.t list        [@default []];
  sticker:          Sticker.t option      [@default None];
  viewOnce:         bool;
  reaction:         Reaction.t option     [@default None];
  remoteDelete:     RemoteDelete.t option [@default None];
} [@@ deriving of_yojson]
