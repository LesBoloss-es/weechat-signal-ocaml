type t = {
  emoji:               string;
  remove:              bool;
  targetAuthor:        Address.t;
  targetSentTimestamp: Int64.t;
} [@@deriving of_yojson]
