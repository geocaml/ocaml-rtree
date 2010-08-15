.PHONY: all
all:
	make -C lib
	make -C lib_test

install:
	make -C lib install

reinstall:
	make -C lib reinstall

clean:
	make -C lib clean
	make -C lib_test clean