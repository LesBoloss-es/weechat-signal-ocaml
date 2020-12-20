type t = {
  name:                  string option [@default None];
  address:               Address.t;
  color:                 string option [@default None];
  profileKey:            string option [@default None];
  messageExpirationTime: int;
  inboxPosition:         int option    [@default None];
} [@@deriving of_yojson, show]


(** {2 Getters} *)

let nice_name c =
  match c.name, c.address.number, c.address.uuid with
  | Some n, _, _
  | None, Some n, _
  | None, None, Some n -> Ok n
  | _ -> Error "No nice_name for this contact"
