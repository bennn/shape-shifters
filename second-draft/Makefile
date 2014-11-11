.PHONY: test

all: test java scala paper

test:
	ocamlbuild  test/run_test.d.byte
	./run_test.d.byte

clean:
	ocamlbuild -clean 
