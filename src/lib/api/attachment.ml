type t = {
  contentType:    string;
  id:             string;
  key:            string;
  size:           int option    [@default None];
  digest:         string option [@default None];
  voiceNote:      bool;
  width:          int;
  height:         int;
  caption:        string option [@default None];
  blurhash:       string option [@default None];
  customFilename: string option [@default None];
  storedFilename: string option [@default None];
  filename:       string option [@default None];
} [@@deriving yojson]
