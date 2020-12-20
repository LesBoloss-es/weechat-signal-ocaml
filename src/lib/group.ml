open Api

type t =
  | V1 of GroupInfo.t
  | V2 of GroupV2Info.t


(** {2 Debug pretty printing} *)

let pp fmt = function
  | V1 gi -> Format.fprintf fmt "GroupV1(%a)" GroupInfo.pp gi
  | V2 gi -> Format.fprintf fmt "GroupV2(%a)" GroupV2Info.pp gi


(** {2 Some getters} *)

let id g = match g with
  | V1 gi -> Ok (gi.groupId)
  | V2 gi ->
    match gi.id with
    | Some id -> Ok id
    | None -> Helpers.error "Group without an id: %a" pp g

let title g =
  let title_opt = match g with
    | V1 gi -> gi.name
    | V2 gi -> gi.title
  in
  match title_opt with
    | Some title -> Ok title
    | None -> Helpers.error "Group without a title: %a" pp g

