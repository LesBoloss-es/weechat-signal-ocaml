open Api
open Helpers.Syntax

module ToJson = struct
  let string s = `String s

  let list elt_to_json xs =
    `List (List.map elt_to_json xs)
end

type destination =
  | Address of Address.t
  | Group of Group.t

let opt_add name value f assoc =
  match value with
  | Some x -> (name, f x) :: assoc
  | None -> assoc

let send
    ?(messageBody=None)
    ?(attachments=[])
    ?(quote=None)
    ~username
    ~destination
    (sock: Socket.t)
  =
    let* destination =
      match destination with
      | Address a -> Ok ("recipientAddress", Address.to_yojson a)
      | Group g -> let+ id = Group.id g in ("recipientGroupId", `String id)
    in
    let assoc =
      [("username", `String username); destination]
      |> opt_add "messageBody" messageBody ToJson.string
      |> opt_add "quote" quote Quote.to_yojson
    in
    let assoc = match attachments with
      | [] -> assoc
      | _ ->
        ("attachments", ToJson.list Attachment.to_yojson attachments) :: assoc
    in
    let assoc = ("type", `String "send") :: assoc in
    Socket.print_json sock (`Assoc assoc)
