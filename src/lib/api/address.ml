type t = {
  number: string option;
  uuid: string option;
  relay: string option;
} [@@deriving of_yojson, show]


(** {2 Getters} *)

let number addr = match addr.number with
  | Some n -> Ok n
  | None -> Helpers.error "No number of address %a" pp addr
