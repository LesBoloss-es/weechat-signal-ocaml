type t = {
  number: string option;
  uuid: string option;
  relay: string option;
} [@@deriving of_yojson]
