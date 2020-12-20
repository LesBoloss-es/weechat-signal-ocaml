type t = {
  groupId:  string;
  members:  Address.t list option [@default None];
  name:     string option         [@default None];
  typ:      string                [@key "type"];
  avatarId: Int64.t option        [@default None];
} [@@deriving of_yojson]
