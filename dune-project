(lang dune 2.0)

(generate_opam_files true)
(name "weechat-signal-ocaml")
(version "0.1")
(license "GPL-3.0-or-later")
(authors "Kerl <kerl@wkerl.me>")
(maintainers "Kerl <kerl@wkerl.me>")
; (source (github LesBoloss-es/weechat-signal-ocaml))
; (homepage "https://github.com/LesBoloss-es/weechat-signal-ocaml")

(package
 (name weechat-signal-ocaml)
 (synopsis "Weechat plugin for signal written in OCaml")
 (depends ("dune" :build) weechat weechat))
