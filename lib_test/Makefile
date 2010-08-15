OCAMLMAKEFILE=../OCamlMakefile

.PHONY: run
run: dnc
	echo $(GIT_TEST)
	./$(RESULT) -verbose

RESULT= run_test
SOURCES= basic.ml
ANNOTATE=yes

INCDIRS=../lib
LIBS=rtree
PACKS=oUnit extlib
OCAMLFLAGS="-g"

-include $(OCAMLMAKEFILE)
