(lang dune 3.15)

(name NeML)

(generate_opam_files true)

(source
 (github therain7/NeML))

(authors
 "Andrei <therain.i@yahoo.com>"
 "PavlushaSource <pavlushkin.dima1785@gmail.com>")

(maintainers
 "Andrei <therain.i@yahoo.com>"
 "PavlushaSource <pavlushkin.dima1785@gmail.com>")

(license MIT)

(package
 (name NeML)
 (synopsis "NeML compiler")
 (description "A compiler for some non-existent programming language")
 (depends
  ocaml
  dune
  base
  pprint
  stdio
  angstrom
  ppx_deriving
  ppx_sexp_conv
  ppx_expect))
