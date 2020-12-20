type t = {
  name:                  string option [@default None];
  address:               Address.t;
  color:                 string option [@default None];
  profileKey:            string option [@default None];
  messageExpirationTime: int;
  inboxPosition:         int;
} [@@deriving of_yojson, show]


(** {2 Getters} *)

let name c = match c.name with
  | Some n -> Ok n
  | None -> Helpers.error "No name for contact %a" pp c
