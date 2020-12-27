open Weechat_api

let to_weechat =
  let map = [
    "blue",        "069";
    "blue_grey",   "138";
    "brown",       "101";
    "green",       "green";
    "indigo",      "099";
    "light_green", "035";
    "orange",      "202";
    "pink",        "164";
    "purple",      "129";
    "red",         "160";
    "teal",        "031";
    "ultramarine", "027";
  ] in
  fun (signal_color: string) : string ->
    match List.assoc_opt signal_color map with
    | Some c -> Weechat.color c
    | None -> ""
