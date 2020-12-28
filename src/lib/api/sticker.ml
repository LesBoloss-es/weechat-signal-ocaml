type t = {
  attachment: Attachment.t;
  packID:     string;
  packKey:    string;
  stickerID:  int;
} [@@deriving of_yojson]
