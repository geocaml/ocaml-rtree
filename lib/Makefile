ANNOTATE= yes
SOURCES= rtree.mli rtree.ml 
RESULT= rtree
LIBINSTALL_FILES= META rtree.cma rtree.cmxa rtree.a rtree.cmi
# LIB_PACK_NAME= rtree
PACKS= unix extlib		# XXX

.PHONY: all
all: ncl bcl dcl rtree.cma rtree.cmxa
	@ :

install: libinstall
uninstall: libuninstall
reinstall: libuninstall libinstall

OCAMLMAKEFILE= ../OCamlMakefile
include $(OCAMLMAKEFILE)
