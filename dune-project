(lang dune 3.6)

(name rtree)

(using mdx 0.3)

(generate_opam_files true)
(source (github geocaml/ocaml-rtree))
(license BSD-3-Clause)
(authors "Marius A. Eriksen" "Patrick Ferris")
(maintainers "patrick@sirref.org")

(package
 (name rtree)
 (synopsis "A pure OCaml R-Tree implementation")
 (tags ("spatial" "index"))
 (description "This implements a simple, functional R-Tree library in pure OCaml with support for efficient bulk loading of values.")
 (depends
  (repr (>= 0.4.0))
  (bechamel :with-test)
  (vg :with-test)
  (ounit2 :with-test)
  (mdx (and :with-test (>= 2.2.0)))))