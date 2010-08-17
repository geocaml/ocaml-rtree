
FILES= \
  rtree.cma rtree.cmxa rtree.a \
  rtree.mli rtree.cmi \
  rtree_f.cma rtree_f.cmxa rtree_f.a \
  rtree_f.mli rtree_f.cmi \

BFILES=$(addprefix _build/lib/,$(FILES))

.PHONY: all
all:
	ocamlbuild -I lib rtree.cma rtree.cmxa rtree_f.cma rtree_f.cmxa

test:
	ocamlbuild -I lib lib_test/basic.native
	./basic.native; echo

.PHONY: all
doc:
	ocamlbuild -no-links lib/doc.docdir/index.html

.PHONY: install
install:
	ocamlfind install rtree lib/META $(BFILES)

.PHONY: uninstall
uninstall:
	ocamlfind remove rtree

.PHONY: reinstall
reinstall: all uninstall install

.PHONY: clean
clean:
	ocamlbuild -clean
