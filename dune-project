(lang dune 3.6)

(using mdx 0.3)

(generate_opam_files true)
(source (github geocaml/ocaml-rtree))
(license BSD-3-Clause)
(authors "Marius A. Eriksen" "Patrick Ferris")
(maintainers "patrick@sirref.org")

(package
 (name rtree)
 (synopsis "A pure OCaml R-Tree implementation")
 (depends
  repr
  (bechamel :with-test)
  (vg :with-test)
  (ounit2 :with-test)
  (mdx :with-test)))