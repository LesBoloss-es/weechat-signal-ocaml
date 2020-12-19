include Yojson.Basic


(** {2 Error reporting} *)

let error fmt =
  Format.kasprintf (fun s -> Error s) fmt

let parse_error fmt =
  Format.kasprintf
    (fun s -> error "Parse error: %s" s)
    fmt


(** {2 Assocative list helpers} *)

let as_assoc = function
  | `Assoc assoc -> Ok assoc
  | json -> parse_error "Not an assoc: %a" pp json

let assoc_get name assoc =
  match List.assoc_opt name assoc with
  | Some x -> Ok x
  | None -> Error "not found: %s"


(** {2 String helpers} *)

let as_string = function
  | `String s -> Ok s
  | json -> parse_error "Not a string: %a" pp json
