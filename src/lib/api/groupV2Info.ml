type t = {
  id:                 string option         [@default None];
  inviteLinkPassword: string option         [@default None];
  masterKey:          string option         [@default None];
  members:            Address.t list option [@default None];
  pendingMembers:     Address.t list option [@default None];
  requestingMembers:  Address.t list option [@default None];
  revision:           int;
  timer:              int;
  title:              string option         [@default None];
} [@@deriving of_yojson, show]
