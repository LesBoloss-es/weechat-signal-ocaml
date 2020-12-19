(lang dune 2.0)

(generate_opam_files true)
(name "signal")
(version "0.1")
(license "GPL-3.0-or-later")
(authors "Kerl <kerl@wkerl.me>")
(maintainers "Kerl <kerl@wkerl.me>")
; (source (github LesBoloss-es/ocaml-signal))
; (homepage "https://github.com/LesBoloss-es/ocaml-signal")

(package
 (name signal)
 (synopsis "Tiny signald library in OCaml")
 (depends ("dune" :build)))
