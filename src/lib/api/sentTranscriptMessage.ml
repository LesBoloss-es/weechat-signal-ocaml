type status_map = (string * bool) list

let status_map_of_yojson json =
  let open Helpers.Syntax in
  match json with
  | `Assoc assoc ->
    Helpers.try_list_map
      (fun (id, b) ->
        let+ b = [%of_yojson: bool] b in
        (id, b))
      assoc
  | _ -> Error "status_map"

type t = {
  destination:              Address.t option [@default None];
  expirationStartTimestamp: Int64.t;
  isRecipientUpdate:        bool;
  message:                  DataMessage.t;
  timestamp:                Int64.t;
  unidentifiedStatus:       status_map       [@default []];
} [@@deriving of_yojson]
