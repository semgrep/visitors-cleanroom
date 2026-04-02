(lang dune 3.0)
(name visitors-cleanroom)
(generate_opam_files true)

(source (github semgrep/visitors-cleanroom))
(license LGPL-2.1-only-with-OCaml-LGPL-linking-exception)
(authors "Semgrep Inc.")
(maintainers "Semgrep Inc.")

(package
 (name visitors-cleanroom)
 (synopsis "Clean-room implementation of the VisitorsRuntime module")
 (description "A clean-room implementation of the VisitorsRuntime module from the visitors OCaml PPX deriver for generating visitor-pattern classes.")
 (depends
  (ocaml (>= 4.14))
  (ppxlib (and (>= 0.22.0) (< 0.36.0)))
  (ppx_deriving (>= 5.0))
  result))
