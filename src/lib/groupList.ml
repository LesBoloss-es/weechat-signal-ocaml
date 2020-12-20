type t = {
  groups:   GroupInfo.t list;
  groupsv2: GroupV2Info.t list;
} [@@deriving of_yojson]
