type t = {
  length: int;
  start:  int;
  uuid:   string;
} [@@deriving yojson]
