type t = {
  attachments: QuotedAttachment.t list [@default []];
  author:      Address.t;
  id:          Int64.t;
  mentions:    Mention.t list          [@default []];
  text:        string;
} [@@deriving yojson]
