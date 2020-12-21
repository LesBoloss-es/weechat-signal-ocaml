type t = {
  branch:  string;
  commit:  string;
  name:    string;
  version: string;
} [@@deriving of_yojson]
