open Syntax


type t = {
  name: string;
  uuid: string;
  number: string
}


(** {2 Pretty printing} *)

let nice_name c =
  if c.name != ""
  then c.name
  else c.number

let pp fmt c =
  Format.pp_print_string fmt (nice_name c)


(** {2 Signald parsing} *)

let parse json =
  let* assoc = Json.as_assoc json in
  let* address = Json.assoc_get "address" assoc >>= Json.as_assoc in
  let+ uuid = Json.assoc_get "uuid" address >>= Json.as_string
  and+ number = Json.assoc_get "number" address >>= Json.as_string in
  let name =
    match Json.assoc_get "name" assoc >>= Json.as_string with
    | Ok name -> name
    | Error _ -> ""
  in
  {name; number; uuid}

let parse_contact_list json =
  let* list = Json.as_list json in
  try_list parse list
