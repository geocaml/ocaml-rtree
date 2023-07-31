(lang dune 3.6)

(formatting disabled)
(generate_opam_files true)
(source (github geocaml/ocaml-rtree))
(license BSD3)
(authors "Marius A. Eriksen" "Patrick Ferris")
(maintainers "patrick@sirref.org")

(package
 (name rtree)
 (synopsis "A pure OCaml R-Tree implementation")
 (depends
  (bechamel :with-test)
  (ounit :with-test)))