open Syntax

type version = V1 | V2

type t = {
  version: version;
  title: string;
  id: string;
}

(** {2 Pretty printing} *)

(** Mostly for debugging purpose *)

let pp_version fmt = function
  | V1 -> Format.pp_print_string fmt "V1"
  | V2 -> Format.pp_print_string fmt "V2"

let pp fmt g =
  Format.fprintf fmt
    "Group%a(%s, %s)"
    pp_version g.version
    g.id
    g.title


(** {2 Signald parsing} *)

let parse_v1 json =
  let* assoc = Json.as_assoc json in
  let* title = Json.assoc_get "name" assoc in
  let* id = Json.assoc_get "groupId" assoc in
  let+ title = Json.as_string title
  and+ id = Json.as_string id in
  {title; version = V1; id}

let parse_v2 json =
  let* assoc = Json.as_assoc json in
  let* title = Json.assoc_get "title" assoc in
  let* id = Json.assoc_get "id" assoc in
  let+ title = Json.as_string title
  and+ id = Json.as_string id in
  {title; version = V2; id}

let parse_list elt_parser =
  let rec parse_with_early_exit parsed = function
    | [] -> Ok parsed
    | x :: xs ->
      begin match elt_parser x with
      | Ok x -> parse_with_early_exit (x :: parsed) xs
      | Error _ as err -> err
      end
  in
  parse_with_early_exit []

let parse_group_list json =
  let* assoc = Json.as_assoc json in
  let+ groupsv1 =
    or_else (Json.assoc_get "groups" assoc) (Ok (`Assoc []))
    >>= Json.as_list
    >>= parse_list parse_v1
  and+ groupsv2 =
    or_else (Json.assoc_get "groupsv2" assoc) (Ok (`Assoc []))
    >>= Json.as_list
    >>= parse_list parse_v2
  in
  groupsv1 @ groupsv2
