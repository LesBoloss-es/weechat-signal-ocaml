type t = {
  contentType: string;
  fileName:    string;
  thumbnail:   Attachment.t option [@default None];
} [@@deriving yojson]
