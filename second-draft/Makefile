.PHONY: test

all:
	ocamlc definitions.ml subtype.ml well_formed.ml

java:
	ocamlbuild test/java.util/run_java.d.byte
	./run_java.d.byte

test:
	ocamlbuild test/runall.d.byte
	./runall.d.byte

clean:
	ocamlbuild -clean 
