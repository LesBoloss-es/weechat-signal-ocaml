type t = {
  number: string option [@default None];
  uuid:   string option [@default None];
  relay:  string option [@default None];
} [@@deriving yojson, show]


(** {2 Getters} *)

let number addr = match addr.number with
  | Some n -> Ok n
  | None -> Helpers.error "No number of address %a" pp addr
