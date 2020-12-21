type t = {
  action:    string;
  groupId:   string option [@default None];
  timestamp: Int64.t;
} [@@deriving of_yojson]
