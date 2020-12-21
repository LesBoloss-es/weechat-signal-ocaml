type t = {
  groupId:  string;
  members:  Address.t list option [@default None];
  name:     string option         [@default None];
  typ:      string option         [@key "type"] [@default None];
  avatarId: Int64.t option        [@default None];
} [@@deriving of_yojson, show]


let of_yojson json =
  match of_yojson json with
  | Ok x -> Ok x
  | Error _ -> Helpers.error "Could not parse %a as GroupInfo.t" Yojson.Safe.pp json
