type t = {
  sender: Address.t;
  timestamp: Int64.t;
} [@@deriving of_yojson]
