open Weechat_api

let plugin_init () = ()

let () =
  Weechat_plugin.define
    ~plugin_init
    ~plugin_end:Weechat.plugin_end
